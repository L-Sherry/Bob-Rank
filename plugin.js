
"use strict";

const webglplz = (canvas) => {
	const gl = canvas.getContext("webgl");
	return gl;
};

const assert_ok = (context, thingname, thing, param) => {
	const res = context[`get${thingname}Parameter`](thing, context[param]);
	if (res)
		return true;
	console.assert(res, context[`get${thingname}InfoLog`](thing));
	context[`delete${thingname}`](thing);
	return false;
};

const compile_shader = (context, type, source) => {
	const shader = context.createShader(context[type]);
	context.shaderSource(shader, source);
	context.compileShader(shader);

	if (!assert_ok(context, "Shader", shader, "COMPILE_STATUS"))
		return null;
	return shader;
};


const create_program = (context, shaders) => {
	const program = context.createProgram();
	for (const shader of shaders)
		context.attachShader(program, shader);
	context.linkProgram(program);

	if (!assert_ok(context, "Program", program, "LINK_STATUS"))
		return null;
	return program;
};

const extractlocations = (context, program, attribs, globals) => {
	const ret = {};
	for (const attrib of attribs) {
		ret[attrib] = context.getAttribLocation(program, attrib);
		console.assert(ret[attrib] !== -1);
		context.enableVertexAttribArray(ret[attrib]);
	}
	for (const global of globals) {
		ret[global] = context.getUniformLocation(program, global);
		console.assert(ret[global] !== -1);
	}
	return ret;
};

// Fill a buffer
const fill_buffer = (context, buffer, data, how) => {
	console.assert(buffer !== undefined && data);
	if (data.constructor !== Float32Array)
		data = new Float32Array(data);
	// where are my display lists ?
	context.bindBuffer(context.ARRAY_BUFFER, buffer);
	context.bufferData(context.ARRAY_BUFFER, data, how);
};

// Fill a mostly-constant buffer.
const fill_const_buffer = (context, buffer, data) =>
	fill_buffer(context, buffer, data, context.STATIC_DRAW);
// Fill a buffer used once or almost once
const fill_dynamic_buffer = (context, buffer, data) =>
	fill_buffer(context, buffer, data, context.DYNAMIC_DRAW);

const select_buffer = (context, buffer) => {
	console.assert(buffer !== undefined);
	context.bindBuffer(context.ARRAY_BUFFER, buffer);
};



const quat_to_mat = (r, x, y, z) => {
	// unit quats only. If not, we are zooming by 1/|q|²
	const sqrt2 = Math.sqrt(2);
	r*= sqrt2;
	x*= sqrt2;
	y*= sqrt2;
	z*= sqrt2;

	const xy = x * y;
	const yz = y * z;
	const xz = x * z;
	const xx = x * x;
	const yy = y * y;
	const zz = z * z;
	const rx = r * x;
	const ry = r * y;
	const rz = r * z;
	// basically what happens when multiplying a quat with its conj.
	// https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
	return [
		[1-yy-zz,  xy-rz,  xz+ry, 0],
		[xy+rz, 1-xx-zz, yz - rx, 0],
		[xz - ry, yz + rx, 1-xx - yy, 0],
		[0,0,0,1]
	];
};

// Your off-the-mill matrix multiplication. Slow.
const mulmat = (mat1, mat2) => mat1.map(
	(line1) =>
		line1.reduce(
			(resline, val1, col1) =>
				resline.map((resval, col2) =>
					    resval + val1 * mat2[col1][col2]),
			[0, 0, 0, 0])
);

const mulvec = (mat, vec) =>
	mat.map(l => l.reduce((a,v,i)=> a + v * vec[i],0));

const addvec = (vec1, vec2) => vec1.map((v, i) => v + vec2[i]);

const mulvecnorm = (mat, vec) => {
	const mulled = mulvec(mat, vec);
	return [mulled[0] / mulled[3],
		mulled[1] / mulled[3],
		mulled[2] / mulled[3]];
};
window.mulvecnorm = mulvecnorm;

// rotate_x_angle = 0 -> xy plane is dislayed, pi/2: x(-z) is displayed
const rotate_me = (rotate_x_angle, nudge_angle, nudge_amount) => {
	// so, we want to rotate by angle A around x axis.
	// cos(a/2), sin(a/2), 0, 0
	rotate_x_angle *= 0.5;
	const r = Math.cos(rotate_x_angle);
	// but this is srank, we want to fudge that a bit and not use x axis,
	// but something like (1, cos(nudge)*amount, sin(nudge)*amount)
	// but we gotta normalize that first. since hypot(y,z) = amount,
	// the length of that axis is hypot(1, amount)
	// if we multiply that with our sin(a/2) above, then we have this
	// sin(a/2)/hypot(1, amount) * (1, cos(nudge)*amount, sin(nudge)*amount)
	const x = Math.sin(rotate_x_angle)/Math.hypot(1, nudge_amount);
	const x_nudge_amount = x * nudge_amount;
	const y = x_nudge_amount * Math.cos(nudge_angle);
	const z = x_nudge_amount * Math.sin(nudge_angle);
	return quat_to_mat(r, x, y, z);
};

// Change the matrix to apply a translation before the matrix transformation.
// i.e. this does matrix = matrix * translation_matrix
// this assumes matrix has [0, 0, 0, 1] as last line (i.e. no projection)
const translate_matrix_before = (matrix, x, y, z) => {
	// [a, b, c, x']   [1, 0, 0, x]   [a, b, c, ax + by + cz + x']
	// [d, e, f, y'] * [0, 1, 0, y] = [d, e, f, dx + ey + fz + y']
	// [g, h, j, z']   [0, 0, 1, z]   [g, h, j, gx + hy + jz + z']
	// [0, 0, 0, 1 ]   [0, 0, 0, 1]   [0, 0, 0, 1]
	// You're ENGINEERS ! You don't write fors, you UNROLL THAT LOOP !
	// -- Teacher
	const dot_me = l => l[3] += l[0] * x + l[1] * y + l[2] * z;
	dot_me(matrix[0]);
	dot_me(matrix[1]);
	dot_me(matrix[2]);
};

// Change the matrix to apply a translation after the matrix transformation.
// i.e. this does matrix = translation_matrix * matrix
// this assumes matrix has [0, 0, 0, 1] as last line (i.e. no projection)
const translate_matrix = (matrix, x, y, z) => {
	// [1, 0, 0, x]   [a, b, c, x']   [a, b, c, x + x']
	// [0, 1, 0, y] * [d, e, f, y'] = [d, e, f, y + y']
	// [0, 0, 1, z]   [g, h, j, z']   [g, h, j, z + z']
	// [0, 0, 0, 1]   [0, 0, 0, 1 ]   [0, 0, 0, 1]
	matrix[0][3] += x;
	matrix[1][3] += y;
	matrix[2][3] += z;
};

// throw to xy wall at z = min,
// note that's i'm more or less assuming the game's coordinate system here.
// i.e. x goes to left, y goes down and z impales you
// but we still need to convert to the dreadfulded opengl screen coordinates,
// where bottom left is (-1, -1) and top right is (1, 1) and the z is clipped
// between -1 and 1.
//
// zmin and zmax must be negative, of course, since we are looking at negative
// z values.
const throw_at_wall = (fov, ratio, zmin, zmax) => {
	// what do we want here ?
	//
	// we want to scale x and y by the distance from 0 to the z plane.
	// but wait, z goes toward us, which mean, lower coordinates are
	// farther than higher coordinates.
	//
	// The thing is that our camera will typically be at high heights,
	// looking at z values below itself. since the camera is shifted at
	// (0,0,0), this means that the z coordinates of things we look at is
	// negative. So, we don't divide by z, we divide by -z.
	//
	// such as x = tan(fov/2) and z = 1 goes to projected x = 1
	// and y = tan(fov/2) / ratio and z = 1 goes to projected y = -1
	// for z: we want to scale [zmin, zmax] to [-1,1], apparently that's
	// what the depth buffer want.
	//
	// where to start ?
	//
	const tanfov = Math.tan(fov/2);
	const taninverse = 1/tanfov;

	// scale x and y by -1 / z (trivial utilization of fourth coordinate)
	// [[1, 0, 0, 0]
	//  [0, 1, 0, 0]
	//  [0, 0, 1, 0]
	//  [0, 0, -1, 0]]
	//
	// (tanfov, tanfov/ratio, 1, 0) must be rendered at (1, -1),
	// so divide x by tanfov and y by -tanfov/ratio
	//
	// [[1/tanfov, 0, 0, 0]
	//  [0, -ratio/tanfov, 0, 0]
	//  [0, 0, 1, 0]
	//  [0, 0, -1, 0]]
	//
	// now, zmin must be mapped to -1 and zmax to 1
	// this would have been simple, if it wasn't for the fact that z
	// is ALSO divided by the fourth coordinate, which is -z.
	//
	// so, zmin must be mapped to a value, which, when divided by -zmin,
	//     maps to -1... so zmin must map to -(-zmin) = zmin
	// and zmax must be mapped to a value, which, when divided by -zmax,
	//     maps to 1... so zmax must map to -zmax
	//
	// a stupid affine function. zmax-zmin must be mapped to a difference
	// of -zmax - zmin, so multiply by -(zmax + zmin)/(zmax - zmin)
	// aka (zmax + zmin)/(zmin - zmax)
	const divisor = 1/(zmin-zmax);
	//
	// and zmax must still map to -zmax, but now it maps to
	// zmax * (zmax + zmin)/(zmin-zmax), so just add
	// -zmax - zmax * (zmax + zmin)/(zmin-zmax) to the result
	// = (-zmin * zmax + zmax² - zmax² - zmax * zmin)/(zmin-zmax)
	// = -2 zmin*zmax / (zmin-zmax)
	return [
		[taninverse, 0, 0, 0],
		[0, -ratio*taninverse, 0, 0],
		[0, 0, (zmax+zmin)*divisor, -2*zmax*zmin*divisor],
		[0, 0, -1, 0]
	];
	// see ? math is easy !
};

const stuff_matrix_to_uniform = (context, mat_location, matrix) => {
	// let's pray it's the same order as mine...
	// ...of course NOT. What was I thinking ?!
	const flattened = Float32Array.of(
		matrix[0][0], matrix[1][0], matrix[2][0], matrix[3][0],
		matrix[0][1], matrix[1][1], matrix[2][1], matrix[3][1],
		matrix[0][2], matrix[1][2], matrix[2][2], matrix[3][2],
		matrix[0][3], matrix[1][3], matrix[2][3], matrix[3][3]
	);

	// if true was used instead of false, this would have transposed the
	// matrix, simplifying this a lot, but non~. OpenGL ES forbid it.
	context.uniformMatrix4fv(mat_location, false, flattened);
};

// This trashes your current ACTIVE_TEXTURE with it. hope that's ok with you !
const create_texture = (context, image) => {
	const texture = context.createTexture();
	context.bindTexture(context.TEXTURE_2D, texture);
	context.texImage2D(context.TEXTURE_2D, 0, context.RGBA,
			   context.RGBA, context.UNSIGNED_BYTE, image);
	// clamp to the edges, it's not like we will wrap textures or whatever.
	// quite the contrary. Also webgl1 requires this for non-power of two ?
	context.texParameteri(context.TEXTURE_2D, context.TEXTURE_WRAP_S,
			      context.CLAMP_TO_EDGE);
	context.texParameteri(context.TEXTURE_2D, context.TEXTURE_WRAP_T,
			      context.CLAMP_TO_EDGE);
	// too small ? use linear... not sure how much time this will happen.
	context.texParameteri(context.TEXTURE_2D, context.TEXTURE_MIN_FILTER,
			      context.LINEAR);
	// show me those pixels, bro
	context.texParameteri(context.TEXTURE_2D, context.TEXTURE_MAG_FILTER,
			      context.NEAREST);
	return texture;
};

const set_vertex_format = (context, location, components, stride, offset) => {
	console.assert(location !== undefined);
	context.vertexAttribPointer(location, components, context.FLOAT,
				    false, stride * 4, offset * 4);
};

const assign_texture = (context, texture) => {
	console.assert(texture);
	context.bindTexture(context.TEXTURE_2D, texture);
};

const draw_triangles = (context, from, size) =>
	context.drawArrays(context.TRIANGLES, from, size);

// maybe there is a way to iteratize it ?
const forEachBackward = (array, callback, from) => {
	if (from === undefined)
		from = array.length - 1;
	for (let idx = from; idx >= 0; --idx)
		callback(array[idx], idx, array);
};

class TextureTrove {
	constructor(context) {
		this._textures = {};
		this._wanted = new Set();
		this._context = context;
	}
	// add and mark as wanted.
	add(path, image) {
		this._wanted.add(path);
		let texture = this._textures[path];
		if (texture === undefined) {
			texture = create_texture(this._context, image);
			this._textures[path] = texture;
		}
		return texture;
	}
	get(path) {
		const ret = this._textures[path];
		console.assert(ret, "getting unwanted texture");
		return ret;
	}
	// delete everything not wanted since last cleanup()
	cleanup() {
		for (const path in this._textures) {
			if (this._wanted.has(path))
				continue;
			this._context.deleteTexture(this._textures[path]);
			delete this._textures[path];
		}
		this._wanted.clear();
	}
}

// Geometry helpers
class BobGeo {
	static _quad_horizontal(x, y, z, size_x, size_y, shift_x, shift_y) {
		return [ x + shift_x, y + shift_y - size_y, z];
	}
	static _quad_vertical(x, y, z, size_x, size_y, shift_x, shift_y) {
		return [ x + shift_x, y, z + size_y - shift_y];
	}
	static make_quad_raw(x, y, z, quad_type, size_x, size_y, coords) {
		const ret = {};
		let transform;
		switch (quad_type) {
		case "horizontal":
			transform = BobGeo._quad_horizontal.bind(null, x, y, z,
								 size_x,
								 size_y);
			break;
		case "vertical":
			transform = BobGeo._quad_vertical.bind(null, x, y, z,
							       size_x, size_y);
			break;
		default:
			throw "unknown quad type";
		}
		for (const pos in coords)
			ret[pos] = transform(coords[pos][0], coords[pos][1]);
		return ret;
	}
	static make_quad_vertex(basex, basey, basez, quad_type,
				size_x, size_y) {
		// base is ... low x, high y, low z. good ?
		// in fact, it will be hard to do otherwise.
		return BobGeo.
			make_quad_raw(basex, basey, basez, quad_type,
				      size_x, size_y,
				      { topleft: [ 0, 0 ],
					topright: [ size_x, 0 ],
					bottomleft: [ 0, size_y ],
					bottomright: [ size_x, size_y] });
	}
	static make_rotated_quad_vertex(base, quad_type, size,
					pivot, rotation, scale) {
		// rotation interferes with the fact that bottomleft is 0 ?
		const cosine = Math.cos(rotation);
		const sine = Math.sin(rotation);
		const rotate_mat = [ // scaling applied before rotation
			[ scale[0] * cosine, -scale[1] * sine ],
			[ scale[0] * sine, scale[1] * cosine ]
		];
		const pivot_shift =
			// shift pivot to (0,0), rotate, then shift to pivot
			// essentially calculates the trans matrix 3rd column
			addvec(pivot, mulvec(rotate_mat, pivot.map(x => -x)));

		// remember bottomleft is at (0,0) due to make_quad_raw.
		// how about "bullshit"
		const topleft = pivot_shift;
		const left_to_right_shift = mulvec(rotate_mat, [size[0], 0]);
		const top_to_bottom_shift = mulvec(rotate_mat, [0, size[1]]);
		const topright = addvec(topleft, left_to_right_shift);
		const bottomleft = addvec(topleft, top_to_bottom_shift);
		const bottomright = addvec(bottomleft, left_to_right_shift);

		return BobGeo.make_quad_raw(...base, quad_type,
					    size[0], size[1],
					    { topleft, topright,
					      bottomleft, bottomright});
	}
	static make_quad_tex(tile_x_pos, tile_y_pos, total_width, total_height,
			     tile_size_x, tile_size_y) {
		const left = tile_x_pos / total_width;
		const right = left + tile_size_x / total_width;
		const top_ = tile_y_pos / total_height;
		const bottom = top_ + tile_size_y / total_height;
		//console.assert(left >= 0 && right <= 1);
		//console.assert(top_ >= 0 && bottom <= 1);
		return { topleft: [ left, top_ ],
			 topright: [ right, top_ ],
			 bottomleft: [ left, bottom ],
			 bottomright: [ right, bottom ] };
	}

	// Interleave those quads as TRIANGLES (not TRIANGLE_STRIP)
	static interleave_triangles(destination, ... quads) {
		// let's do it clockwise, even if, given our circonstances,
		// we do not give any actual fuck.
		//
		// top left, bottom right, bottom left
		// top left, top right, bottom right

		for (const pos of ["topleft", "bottomright", "bottomleft",
				   "topleft", "topright", "bottomright"])
			for (const quad of quads) {
				console.assert(!isNaN(quad[pos][0]));
				destination.push(...quad[pos]);
			}
	}

	/*
	static make_triangle(basex, basey, basez, quad_type, triangles, shift) {
		// clockwise:
		//
		// top left, bottom right, bottom left
		// top left, top right, bottom right

		const do_triangle = (shiftx, shifty, shiftz) => {
			triangles.push(basex + shiftx,
				       basey + shifty,
				       basez + shiftz);
		};
		switch (quad_type) {
		case "horizontal":
			do_triangle(0, -shift, 0);
			do_triangle(shift, 0, 0);
			do_triangle(0, 0, 0);

			do_triangle(0, -shift, 0);
			do_triangle(shift, -shift, 0);
			do_triangle(shift, 0, 0);
			break;
		case "vertical":
			do_triangle(0,0,shift);
			do_triangle(shift,0,0);
			do_triangle(0,0,0);

			do_triangle(0,0,shift);
			do_triangle(shift,0,shift);
			do_triangle(shift,0,0);
			break;
		}
	}
	static make_texture(tile_x_pos, tile_y_pos, total_width, total_height,
			    tile_size, st_coords) {
		// use same convention as make_triangle.
		// also, textures are screen-oriented.
		const left = tile_x_pos / total_width;
		const right = left_x + tile_size / total_width;
		const top_ = tile_y_pos / total_height;
		const bottom = top_y + tile_size / total_width;

		st_coords.push(
			left, top_,
			right, bottom,
			left, bottom,
			left, top_,
			right, top_,
			right, bottom_);
	}*/
}

class BobRenderable {
	constructor(context, vertex_location, text_coord_location) {
		this.context = context;
		this.vertex_location = vertex_location;
		this.text_coord_location = text_coord_location;
		this.texture_trove = new TextureTrove(this.context);
		this.buf = context.createBuffer();
		this.textures_ranges = [];
	}

	render() {
		select_buffer(this.context, this.buf);
		// three floats for the position, two floats for texture pos
		// total = 5
		set_vertex_format(this.context, this.vertex_location, 3, 5, 0);
		set_vertex_format(this.context, this.text_coord_location, 2,
				  5, 3);

		for (const textrange of this.textures_ranges) {
			console.assert(textrange.texture && textrange.size
				       && textrange.start !== undefined);
			assign_texture(this.context, textrange.texture);
			draw_triangles(this.context, textrange.start,
				       textrange.size);
		}
	}
}


class BobMap extends BobRenderable {
	constructor(context, vertex_location, text_coord_location) {
		super(context, vertex_location, text_coord_location);
	}
	steal_map() {
		// AAHHHH where are my quads ? they take away my display lists,
		// and now they take away my quads too ?
		// i have to do TRIANGLES ? TRIANGLES SUCKS ! QUADROGUARD FTW !
		const everything = [];
		let i = 0;

		// should probably migrate to vertex indexes. at least they
		// kept this.

		const handle_tile = (x, y, z, tileno, tile_size,
				     tiles_per_line, tiles_per_col) => {
			const tile_type = /* magic ...*/ "horizontal";

			// make_quad_vertex want high y
			const quad_vertex
				= BobGeo.make_quad_vertex(x, y + tile_size,
							  z, tile_type,
							  tile_size, tile_size);

			/*
			BobGeo.make_triangle(x, y, z, tile_type,
					     everything,
					     tile_size);*/

			const true_tiles_per_line = Math.floor(tiles_per_line);
			const tile_x = tileno % true_tiles_per_line;
			const tile_y = Math.floor(tileno / true_tiles_per_line);

			const quad_st_coord
				= BobGeo.make_quad_tex(tile_x, tile_y,
						       tiles_per_line,
						       tiles_per_col, 1, 1);
			BobGeo.interleave_triangles(everything,
						    quad_vertex, quad_st_coord);
			// 6 things were added by interleave_triangles().
			i+=6;
		};

		const handle_one_map_tiles = (map, z) => {
			const tilesize = map.tilesize;
			const width = map.tiles.width;
			const height = map.tiles.height;
			const tiles_per_line = width / map.tilesize;
			// if it's float, it's still good
			const tiles_per_col = height / map.tilesize;

			// high y values are closer to you...
			forEachBackward(map.data, (line, y) => {
				// while the tile at map.data[0][0] height 0
				// stores (0,0,0), the tile at map.data[0][0]
				// height z is rendered at the same place in 2d
				// space.  This mean that its actual coordinate
				// is (0, z, z).
				const shifted_y = y * tilesize + z;
				line.forEach((tile, x) => {
					if (!tile)
						return;
					handle_tile(x * tilesize, shifted_y,
						    z, tile - 1,
						    tilesize,
						    tiles_per_line,
						    tiles_per_col);
				});
			});
		};

		const textures_ranges = this.textures_ranges;
		textures_ranges.length = 0;
		let current_texture = {start:-42};
		// should probably reorder by texture, too ? or nobody cares
		// because it's probable only one is used ?
		const tex_trove = this.texture_trove;

		// iterate from front to back, for crummy performance reasons.
		// also, ig.game.levels is an Object, so this also helps.
		forEachBackward(ig.game.levels, (level,levelno) => {
			// but iterate forwardly here. there are background map
			// who only add details to other maps... or import
			// tiles from other tilesetz
			for (const map of level.maps) {
				if (map.tiles.path !== current_texture.path) {
					// past-the-end, actually.
					current_texture.size
						= i - current_texture.start;
					current_texture = {path: map.tiles.path,
							   start: i};
					textures_ranges.push(current_texture);
					const img = map.tiles.data;
					tex_trove.add(map.tiles.path, img);
				}
				// The Z Fighting War has begun.
				// Only the strongest will survive.
				// Which mean, not you. Sorry, map.
				// but entities are stronger than you and must
				// be displayed on top of you.
				handle_one_map_tiles(map, level.height - 0.5);
			}
		}, ig.game.maxLevel - 1);
		current_texture.size = i - current_texture.start;
		tex_trove.cleanup();
		textures_ranges.forEach(e => e.texture = tex_trove.get(e.path));

		fill_const_buffer(this.context, this.buf, everything);

		// now i have the map ! time to find the treasure !
	}
}

// The first of many.
const path_overrides = {
	media: {
		entity: {
			effects: {
				"lighter-particle.png": "ground",
				"lighter-particle-big.png": "ground"
			}
		}
	}
};

const walk_break_on_first = (object, path) => {
	for (const component of path)
		if (object[component])
			object = object[component];
		else if (object instanceof String)
			return object;
		else
			return null;
	return object;
};

class BobEntities extends BobRenderable {
	constructor(context, vertex_location, text_coord_location) {
		super(context, vertex_location, text_coord_location);
	}
	clear() {
		this.sprites_by_texture = {};
		this.textures_ranges.length = 0;
	}
	static do_overrides(cubesprite) {
		// I have LOADS of reserves on how the game classify ground
		// and wall sprites.
		// i will list them ALL HERE ! MUAHAHAHA !
		const path = cubesprite.image.path.split("/");
		const override = walk_break_on_first(path_overrides, path);
		if (override)
			return override; // for now.

		if (!cubesprite.size.z)
			return "ground";
		return null;
	}
	prepare_sprites(spritearray) {
		const tex_trove = this.texture_trove;
		const by_texture = this.sprites_by_texture;
		for (const sprite of spritearray) {
			const image = sprite.cubeSprite.image;
			if (!(image && image.path && image.data)) {
				// FIXME: ImageCanvasWrapper will be hard to
				// cache... but it's the majority of sprites ?
				if (!(image instanceof ig.ImageCanvasWrapper))
					console.log("strange sprite");
				continue;
			}
			const path = image.path;

			let textureinfo = by_texture[path];
			if (!textureinfo) {
				const img = image.data;
				const texture = tex_trove.add(path, img);
				textureinfo = by_texture[path] = {
					texture,
					sprites: []
				};
			}
			textureinfo.sprites.push(sprite);
		}
	}
	// get a cubeSprite's source crops.
	// replicate the dreaded calculation of SpriteDrawSlot.draw()
	// without the z clippin' part
	static get_2d_src_crop(cubesprite, ground) {
		const cs = cubesprite;
		let offsetx = cs.gfxCut.left;
		let offsety = 0;
		let sizex
			= cs.size.x - cs.gfxCut.right - cs.gfxCut.left;
		let sizey;

		// yes, there are wall sprites with cs.wallY = cs.size.y.
		// Even if i think they should really be ground sprites.
		// (this includes sweeps)

		if (cs.wallY >= cs.size.y)
			// apply default
			sizey = cs.size.y + cs.size.z;
		else if (ground)
			// amputate wallY from ground
			sizey = cs.size.y - cs.wallY;
		else if (!cs.mergeTop) {
			// remove the ground part
			offsety = cs.size.y - cs.wallY;
			// sizey = size.y + size.z - offsety
			sizey = cs.size.z - cs.wallY;
		} else
			sizey = cs.size.y + cs.size.z;

		offsety = Math.max(cs.gfxCut.top, offsety);
		if (cs.gfxCut.bottom)
			sizey = Math.min(sizex,
					 cs.size.y + cs.size.z
					 - cs.gfxCut.bottom);
		if (cs.flip.x)
			// basically, gfxCut.left and gfxCut.right
			// happens after flipping.
			// basically, we should swap them
			offsetx = cs.gfxCut.right;

		if (sizex <= 0 || sizey <= 0
		    || !cs.scale.x
		    || !cs.scale.y
		    || !cs.alpha)
			return null;

		return { offsetx, offsety, sizex, sizey };
	}
	static get_src_quad_tex(cubesprite, ground) {
		const crop = BobEntities.get_2d_src_crop(cubesprite, ground);
		if (!crop)
			return null;
		let {offsetx, offsety, sizex, sizey } = crop;
		if (cubesprite.flip.x) {
			offsetx += sizex;
			sizex = -sizex;
		}
		if (cubesprite.flip.y) {
			offsety += sizey;
			sizey = -sizey;
		}
		offsetx += cubesprite.src.x;
		offsety += cubesprite.src.y;

		console.assert(!isNaN(offsetx) && !isNaN(offsety)
			       && !isNaN(sizex) && !isNaN(sizey));
		const image = cubesprite.image;

		crop.quad_tex = BobGeo.make_quad_tex(offsetx, offsety,
						     image.width, image.height,
						     sizex, sizey);
		return crop;
	}
	finalize_sprites() {
		const everything = [];
		let i = 0;

		const do_sprite = sprite => {
			const cs = sprite.cubeSprite;
			let is_ground = sprite.ground;
			// i have some reserves on how the game classify ground
			// sprites from wall sprites.
			switch (BobEntities.do_overrides(cs)) {
			case "ground":
				is_ground = true;
				break;
			case "wall":
				is_ground = false;
				break;
			}

			const src = BobEntities.get_src_quad_tex(cs, is_ground);
			if (!src)
				return; // nothing to do here.

			// BobGeo want low x, high y, low z
			let x = cs.pos.x + cs.tmpOffset.x + cs.gfxOffset.x +
				cs.gfxCut.left;
			let y = cs.pos.y + cs.tmpOffset.y + cs.size.y
					 + cs.gfxOffset.y
			let z = cs.pos.z + cs.tmpOffset.z;
			if (is_ground) {
				// the ground part is the top of the sprite.
				z += cs.size.z;
				// if sprite is cut from the bottom, then
				// cut the 'bottom' of our ground.
				y -= cs.gfxCut.bottom;
			} else {
				// set y to yIndex.
				y -= cs.wallY;
				// we have a problems with wallY.
				// if wallY is non zero, then we must be able
				// to render things 'below the ground'
				// we can't do that. so we just raise everything
				// up. what could possibly go wrong ?
				// this is done by doing nothing to z,
				// instead of substracting wallY to it.
				z += cs.gfxCut.bottom;
			}
			const quad_type
				= is_ground ? "horizontal" : "vertical";
			let quad_vertex;
			//if (cs.rotate)
			quad_vertex
				= BobGeo.make_rotated_quad_vertex(
					[x, y, z], quad_type,
					[src.sizex, src.sizey],
					[cs.pivot.x || 0, cs.pivot.y || 0],
					cs.rotate || 0,
					[cs.scale.x, cs.scale.y]);

			BobGeo.interleave_triangles(everything,
						    quad_vertex, src.quad_tex);
			i += 6;
		};


		for (const path in this.sprites_by_texture) {
			const texture = this.sprites_by_texture[path];
			const start_i = i;

			for (const sprite of texture.sprites)
				do_sprite(sprite);

			if (i === start_i)
				continue;

			this.textures_ranges.push({
				texture: texture.texture,
				start: start_i,
				size: i - start_i
			});
		}

		fill_dynamic_buffer(this.context, this.buf, everything);
	}
}

class BobRender {
	constructor() {
		this.context = null;
		this.map = null;
		// FIXME: this should vary over time
		this.nudge_angle = 0;
		this.nudge_intensity = 0;
		this.rotate = Math.PI / 4;
		// for debugging.
		this.debugshift = { x:0, y:0, z:0 };
	}

	setup_canvas(canvas) {

		const ratio = canvas.width / canvas.height;
		// need a real reflection on fov:
		// - don't want rotate - vertical fov/2 to be below 0, otherwise
		// we will have to render the back of cubes
		// - don't want rotate + vertical fov/2 to be above 90°,
		// otherwise there will be black backgrounds to render.
		// but that's the vertical fov, which needs to be calculated
		// from the horizontal fov, which is probably
		// vfov = 2 * atan(ratio * tan(hfov/2))
		const fov = Math.PI * 0.5;
		this.proj_matrix = throw_at_wall(fov, ratio, -20, -500);

		this.context = webglplz(canvas);

		this.vertexshader = compile_shader(this.context,
						   "VERTEX_SHADER", `
		attribute vec4 pos;
		attribute vec2 texcoord;
		uniform mat4 projectmat;
		varying highp vec2 texcoord2;
		void main() {
			gl_Position = projectmat * pos;
			texcoord2 = texcoord;
		}
		`);

		this.fragshader = compile_shader(this.context,
						 "FRAGMENT_SHADER", `
		varying highp vec2 texcoord2;
		uniform sampler2D colorsampler;
		void main() {
			// FIXME: find what control interpolation in there.
			gl_FragColor = texture2D(colorsampler, texcoord2);
			if (gl_FragColor.a == 0.)
				discard;
			// gl_FragColor = vec4(1, /*gl_Position.x*/ 0, 1, 1);
		}
		`);

		this.program = create_program(this.context,
					      [this.vertexshader,
					       this.fragshader]);
		this.locations = extractlocations(this.context, this.program,
						  ["pos", "texcoord"],
						  ["projectmat",
						   "colorsampler"]);

		this.context.useProgram(this.program);

		// Assume that TEXTURE0 is the base color texture
		this.context.uniform1i(this.locations.colorsampler, 0);
		// note: TEXTURE0 is the default ACTIVE_TEXTURE.

		this.context.enable(this.context.DEPTH_TEST);
		// isn't that the default ? ... no, the default is LESS
		// LEQUAL allows us to redraw on the same tile with more
		// details, which is necessary, at least for maps.
		this.context.depthFunc(this.context.LEQUAL);
		// should make this black at some point.
		// (the default is black with alpha = 0)
		// wait, doesn't the game have a variable about it ?
		this.context.clearColor(0, 0, 1, 1); // blue sky (ok ...)
		// note: the default clearDepth is 1

		this.map = new BobMap(this.context, this.locations.pos,
				      this.locations.texcoord);
		this.entities = new BobEntities(this.context,
						this.locations.pos,
						this.locations.texcoord);
	}

	draw_layerz (parent) {
		parent();
		if (!this.map) {
			console.assert(ig.game.maps.length === 0);
			return;
		}
		if (ig.game.mapRenderingBlocked || ig.loading
		    || !(ig.game.maxLevel > 0))
			return;

		this.entities.clear();
		this.entities.prepare_sprites(ig.game.renderer.spriteSlots);
		this.entities.prepare_sprites(ig.game.renderer.guiSpriteSlots);
		this.entities.finalize_sprites();
		this.map.render();
		this.entities.render();
	}

	bind_to_game() {
		const me = this;
		const modulize = (dummyname, deps, func) =>
			ig.module(dummyname).requires(...deps).defines(func);
		modulize("bobrender", ["impact.base.renderer"], () => {
			ig.Renderer2d.inject({
				drawLayers: function () {
					const parent = this.parent.bind(this);
					return me.draw_layerz(parent);
				}
			});
		});

		modulize("bobrender2", ["impact.base.game"], () => {
			const BobRankAddon = ig.GameAddon.extend({
				onPreDraw: function() {
					me.clear_screen_and_everything();
				},
				onLevelLoaded: function() {
					me.map.steal_map();
				}
			});
			ig.addGameAddon(() => new BobRankAddon());
		  });
	}

	clear_screen_and_everything() {
		if (!this.context)
			return;

		const view_matrix = rotate_me(this.rotate, this.nudge_angle,
					      this.nudge_intensity);

		const centerx = ig.game.screen.x + 570 / 2;
		const centery = ig.game.screen.y + 320 / 2;
		let centerz = 0;
		if (ig.game.playerEntity)
			centerz = ig.game.playerEntity.coll.pos.z;
		// move center of screen at (0,0,0)
		translate_matrix_before(view_matrix,
					-centerx + this.debugshift.x,
					-centery + this.debugshift.y,
					-centerz + this.debugshift.z);
		// take the camera back by 100
		translate_matrix(view_matrix,
				 0,
				 0,
				 -200);

		const mulled = mulmat(this.proj_matrix, view_matrix);
		stuff_matrix_to_uniform(this.context,
					this.locations.projectmat,
					mulled);

		this.context.clear(this.context.COLOR_BUFFER_BIT
				   | this.context.DEPTH_BUFFER_BIT);
	}
}

/*
const injector = (object, methodname, func) => {
	if (object.inject)
		// easy way
		return object.inject({ [methodname]: function(... args) {
			func(this.parent.bind(this), ...args);
		});
	// hard way
	const parent = object[methodname].bind(object);
	object[methodname] = (...args) => func(parent, ...args);
}*/

export default class Mod extends Plugin {
	constructor(what) {
		super(what);
	}
	preload() { 	}
	postload() {

		this.renderer = new BobRender();
		this.renderer.bind_to_game();
	}
	main() {
		// debug
		ig.system.canvas.style.margin = "0px";

		this.canvas3d = document.createElement("canvas");
		this.canvas3d.width = 570;
		this.canvas3d.height = 320;
		this.canvas3d.style.marginTop = "320px";
		document.getElementById("game").appendChild(this.canvas3d);

		this.renderer.setup_canvas(this.canvas3d);
	}
}
