// SPDX-Identifier: MIT
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

const getcol = (mat, colnum) => mat.map(line => line[colnum]);
const mulscalar = (vec, scalar) => vec.map(x => x * scalar);

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

const disable_blending = context => {
	context.disable(context.BLEND);
};
const enable_blending = context => {
	context.enable(context.BLEND);
};
const set_blending = (context, equation, func_source, func_dest, alpha) => {
	// OK, here's how it goes:
	// the equation for updating pixelz is:
	// red = source_red * source_red_factor +- red * red_factor
	// same for blue/green/alpha, except alpha equation can be set
	// separatedly from rgb
	// source_red_factor is set by blending function. Each factor can be
	// blended differently for each color using a blending function, but
	// probably our blending function won't use it.
	// (note: probably our color buffer does not have alpha)
	context.blendEquation(context[equation]);
	context.blendFunc(context[func_source], context[func_dest]);
	if (alpha !== undefined)
		context.blendColor(alpha, alpha, alpha, alpha);
};

// add source and destination, like the canvas "lighter"
const blend_lighter = context =>
	set_blending(context, "FUNC_ADD", "ONE", "ONE");

const blend_lighter_constant = (context, constant) => {
	if (constant === 1)
		// good luck alpha one
		blend_lighter(context);
	else
		set_blending(context, "FUNC_ADD",
			     "ONE", "ONE_MINUS_CONSTANT_ALPHA",
			     constant);
};
// about lighter with non-1 alpha, it is defined as follows:
//
// Cs: source color
// Cb: backdrop color (what's in the dest)
// αs: source alpha (defined by globalAlpha...)
// αb: backdrop alpha
// lighter:
// Co = αs x Cs + αb x Cb;
// αo = αs + αb
//

// do alpha blending the way you are used to, with alpha = constant
const blend_constant = (context, constant) => {
	set_blending(context, "FUNC_ADD",
		     "CONSTANT_ALPHA", "ONE_MINUS_CONSTANT_ALPHA", constant);
};

// maybe there is a way to iteratize it ?
const forEachBackward = (array, callback, from) => {
	if (from === undefined) {
		from = array.length - 1;
		console.assert(from !== undefined, "wtf");
	}
	for (let idx = from; idx >= 0; --idx)
		callback(array[idx], idx, array);
};

const sortWithKey = (array, key) => array.sort((a, b) => key(a) - key(b));


class ColorCalculator {
	constructor() {
		this.canvas = document.createElement("canvas");
		this.canvas.width = 1;
		this.canvas.height = 1;
		this.cache = {};
	}
	get(css_color) {
		let ret = this.cache[css_color];
		if (ret)
			return ret;
		const context2d = this.canvas.getContext("2d");
		context2d.fillStyle = css_color;
		context2d.fillRect(0,0,1,1);
		const content = context2d.getImageData(0,0,1,1).data;
		ret = Array.from(content).map(x => x / 255);
		this.cache[css_color] = ret;
		return ret;
	}
}

const color_calculator = new ColorCalculator();

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
	static make_quad_simple(x, y, z, plane_func, size_x, size_y) {
		plane_func = plane_func.bind(null, x, y, z, size_x, size_y);
		const topleft = plane_func(0, 0);
		const topright = plane_func(size_x, 0);
		const bottomleft = plane_func(0, size_y);
		const bottomright = plane_func(size_x, size_y);
		return { topleft, topright, bottomleft, bottomright };
	}
	static make_horizlike_quad(x, y, z, quad_type, size_x, size_y) {
		const ret = BobGeo.make_quad_simple(x, y, z,
						    BobGeo._quad_horizontal,
						    size_x, size_y);
		let zshift = -size_y;
		if (quad_type.startsWith("BOTTOM_WALL_")) {
			quad_type = ("BORDER_" +
				     quad_type.slice("BOTTOM_WALL_".length));
			zshift = size_y;
		}
		switch (quad_type) {
		case "BORDER_SW":
			ret.bottomleft = [ret.topleft[0], ret.topleft[1],
					  z + zshift];
			break;
		case "BORDER_SE":
			ret.bottomright = [ret.topright[0], ret.topright[1],
					   z + zshift];
			break;
		case "BORDER_NW":
		case "SLOPE_EAST_BORDER_NW":
			// FIXME: handle border slopes
			ret.topleft = [ret.bottomleft[0], ret.bottomleft[1],
				       z + zshift];
			break;
		case "BORDER_NE":
		case "SLOPE_WEST_BORDER_NE":
			ret.topright = [ret.bottomright[0], ret.bottomright[1],
					z + zshift];
			break;
		case "SLOPE_WEST":
			ret.topleft[2] += size_y;
			ret.bottomleft[2] += size_y;
			break;
		case "SLOPE_EAST":
			ret.topright[2] += size_y;
			ret.bottomright[2] += size_y;
			break;
		case "SLOPE_NORTH":
			ret.topleft[2] += size_y;
			ret.topright[2] += size_y;
			break;
		case "SLOPE_SOUTH": // exists ?
			ret.bottomleft[2] += size_y;
			ret.bottomright[2] += size_y;
			break;
		}
		return ret;
	}
	static make_vertlike_quad(x, y, z, quad_type, size_x, size_y) {
		let bottomleft;
		let bottomright;

		switch (quad_type) {
		case "BOTTOM_WALL_WEST":
		case "WALL_WEST":
			bottomleft = [x + size_x, y, z];
			bottomright = [x + size_x, y - size_y, z];
			break;
		case "BOTTOM_WALL_EAST":
		case "WALL_EAST":
			bottomleft = [x, y - size_y, z];
			bottomright = [x, y, z];
			break;

		case "WALL_NW":
			bottomleft = [x, y, z];
			bottomright = [x + size_x, y - size_y, z - size_y];
			break;
		case "WALL_NE":
			bottomleft = [x, y - size_y, z - size_y];
			bottomright = [x + size_x, y, z];
			break;

		case "WALL_SOUTH":
			bottomleft = [x + size_x, y, z];
			bottomright = [x, y, z];
			break;

		case "WALL_SW":
			bottomleft = [x + size_x, y, z - size_y];
			bottomright = [x, y - size_y, z];
			break;
		case "WALL_SE":
			bottomleft = [x + size_x, y - size_y, z];
			bottomright = [x, y, z - size_y];
			break;
		}
		const addz = a => [a[0], a[1], a[2] + size_y];
		const topleft = addz(bottomleft);
		const topright = addz(bottomright);
		return { topleft, topright, bottomleft, bottomright };
	}
	static make_quad_vertex(x, y, z, quad_type, size_x, size_y) {
		// base is ... low x, high y, low z. good ?
		// in fact, it will be hard to do otherwise.
		if (quad_type.startsWith("IGNORE_"))
			quad_type = quad_type.slice("IGNORE_".length);
		switch (quad_type) {
		case "WALL_NORTH":
		case "BOTTOM_WALL_NORTH":
			return BobGeo.make_quad_simple(x, y, z,
						       BobGeo._quad_vertical,
						       size_x, size_y);
		case "BORDER_WEST":
		case "BORDER_EAST":
		case "BORDER_NORTH":
		case "BORDER_SOUTH":
		case "BORDER_NW_FLAT":
		case "BORDER_NE_FLAT":
		case "BORDER_SW_FLAT":
		case "BORDER_SE_FLAT":
		case "GROUND":
			return BobGeo.make_quad_simple(x, y, z,
						       BobGeo._quad_horizontal,
					               size_x, size_y);
		case "BOTTOM_WALL_SW":
		case "BOTTOM_WALL_SE":
		case "BOTTOM_WALL_NW":
		case "BOTTOM_WALL_NE":
		case "BORDER_SW":
		case "BORDER_SE":
		case "BORDER_NW":
		case "BORDER_NE":
		case "SLOPE_WEST_BORDER_NE":
		case "SLOPE_EAST_BORDER_NW":
		case "SLOPE_WEST":
		case "SLOPE_EAST":
		case "SLOPE_NORTH":
		case "SLOPE_SOUTH":
			return BobGeo.make_horizlike_quad(x, y, z, quad_type,
							  size_x, size_y);
		case "SLOPE_EAST_WALL_NW":
		case "SLOPE_WEST_WALL_NE":
			// FIXME implement that.
			quad_type = quad_type.slice(0, "SLOPE_EAST".length);
			return BobGeo.make_horizlike_quad(x, y, z, quad_type,
							  size_x, size_y);

		case "BOTTOM_WALL_WEST":
		case "WALL_WEST":
		case "BOTTOM_WALL_EAST":
		case "WALL_EAST":
		case "WALL_NW":
		case "WALL_NE":
		case "WALL_SOUTH":
		case "WALL_SW":
		case "WALL_SE":
			return BobGeo.make_vertlike_quad(x, y, z, quad_type,
							 size_x, size_y);
		default:
			throw "unknown quad_type:" + quad_type;
		}
	}

	// make a quad vertex's positions
	// x,y,z: position of bottom west north corner (yes BOTTOM)
	// quad_type: vertical or horizontal
	// size_x, size_y : size of the quad coordinate space
	// coords: 2d coordinates in the quad coordinate.
	//	   (0,0) is "top left", (size_x, size_y) is "bottom right"
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
	static make_rotated_quad_matrix(rotation, scale, pivot) {
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

		return {pivot_shift, rotate_mat};
	}
	static make_rotated_quad_vertex(base, quad_type, size,
					pivot, rotation, scale) {
		const { pivot_shift, rotate_mat }
			= BobGeo.make_rotated_quad_matrix(rotation, scale,
							  pivot);

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
	//
	static interleave_triangles(destination, quad_type, ... quads) {
		// let's do it clockwise, even if, given our circonstances,
		// we do not give any actual fuck.
		//
		// top left, bottom right, bottom left
		// top left, top right, bottom right
		//
		// this is good and all for everything including NE/SW
		// but NW/SE need other triangles.
		const triangles_ne = ["topleft", "bottomright", "bottomleft",
				      "topleft", "topright", "bottomright"];
		const triangles_nw = ["topleft", "topright", "bottomleft",
				      "topright", "bottomright", "bottomleft"];
		let triangles = triangles_ne;
		if (quad_type.endsWith("_NW") || quad_type.endsWith("_SE"))
			triangles = triangles_nw;

		for (const pos of triangles)
			for (const quad of quads) {
				console.assert(!isNaN(quad[pos][0]));
				destination.push(...quad[pos]);
			}
	}
}

class BobRenderable {
	constructor(context, locations_opaque, locations_blended) {
		this.locations_opaque = locations_opaque;
		this.locations_blended = locations_blended;

		console.assert(locations_opaque.pos !== undefined
			       && locations_opaque.tex_coord !== undefined
			       && locations_blended.pos !== undefined
			       && locations_blended.tex_coord !== undefined
			       && locations_blended.color_blend
					!== undefined);

		this.context = context;
		this.texture_trove = new TextureTrove(this.context);
		this.buf = context.createBuffer();
		// for opaque objects
		this.textures_ranges = [];
		// for alpha blendung
		this.blending_ranges = [];
	}

	render_opaque() {
		select_buffer(this.context, this.buf);
		// three floats for the position, two floats for texture pos
		// total = 5
		const { pos, tex_coord } = this.locations_opaque;

		set_vertex_format(this.context, pos, 3, 5, 0);
		set_vertex_format(this.context, tex_coord, 2, 5, 3);

		for (const textrange of this.textures_ranges) {
			console.assert(textrange.texture && textrange.size
				       && textrange.start !== undefined);
			assign_texture(this.context, textrange.texture);
			draw_triangles(this.context, textrange.start,
				       textrange.size);
		}
	}
	// assumes blending is already enabled
	render_blended() {
		// maybe at one point we will put parameters in there ?
		// or maybe not.
		select_buffer(this.context, this.buf);

		const { pos, tex_coord, color_blend } = this.locations_blended;
		set_vertex_format(this.context, pos, 3, 5, 0);
		set_vertex_format(this.context, tex_coord, 2, 5, 3);

		// let's do some alpha blendong
		for (const blendrange of this.blending_ranges) {
			assign_texture(this.context, blendrange.texture);
			if (blendrange.mode === "normal")
				blend_constant(this.context, blendrange.alpha);
			else if (blendrange.mode === "lighter")
				blend_lighter_constant(this.context,
						       blendrange.alpha);
			else
				throw "unknown blending mode";
			// set the uniform. doing this for each quad costs, but
			// it's still probably less than changing textures all
			// the time.
			// (plus, it applies to at least 6 vertex, so it's
			// 'uniform' enough)
			this.context.uniform4fv(color_blend,
						blendrange.blend_color);
			draw_triangles(this.context, blendrange.start,
				       blendrange.size);
		}
	}
}


class BobMap extends BobRenderable {
	constructor(context, locations_opaque, locations_blended,
		    moretileinfo) {
		super(context, locations_opaque, locations_blended);
		this.moretileinfo = moretileinfo;

		// { type, tile }
		this.heightinfo_line = null;
		this.last_heightinfo = null;

		this.hiddenblocks = [];
		this.layerviews = [];
		this.height_map = [];
		this.height_map_shift = 0;
		this.mapWidth = null;
		this.mapHeight = null;
	}

	steal_map_data(map) {
		this.hiddenblocks.length = 0;
		this.layerviews.length = 0;

		// create new heightmap (could try stealing from map, but
		// not all maps have them)
		// height map is indexed by true x and y and stores the height
		// and the tile type of the higest tile

		// negative height values cause problems with the height map
		// generation, because their top refer to tiles below 0
		this.height_map_shift = 0;
		// and positive height values also cause problems at the bottom
		// of the map...
		let more_y = 0;
		if (map.levels.length > 0) {
			const min_map_z = map.levels[0].height / 16;
			this.height_map_shift = Math.ceil(Math.abs(min_map_z));

			const max_map_z
				= map.levels[map.levels.length - 1].height / 16;
			more_y = Math.ceil(Math.abs(max_map_z));
		}

		this.mapWidth = map.mapWidth;
		this.mapHeight = map.mapHeight;

		const length = map.mapHeight + this.height_map_shift + more_y;
		this.height_map.length = length;
		for (let y = 0; y < this.height_map.length; ++y) {
			const line = new Array(map.mapWidth);
			this.height_map[y] = line;
			for (let x = 0; x < map.mapWidth; ++x)
				line[x] = { z: null, z_bottom: null,
					    type: null };
		}
	}

	static quad_tex(tileno, map_info) {
		const tile_x = tileno % map_info.tiles_per_line;
		const tile_y = Math.floor(tileno / map_info.tiles_per_line);

		const quad_st_coord
			= BobGeo.make_quad_tex(tile_x, tile_y,
					       map_info.tiles_on_line,
					       map_info.tiles_on_col, 1, 1);
		return quad_st_coord;
	}

	// inclusive include the higher bound
	static filter_coord(coord, entities, value, inclusive) {
		inclusive = inclusive ? 1 : 0;
		return entities.filter(entity => {
			const pos = entity.coll.pos[coord];
			const size = entity.coll.size[coord];
			return pos <= value && value < pos + size + inclusive;
		});
	}

	static hidden_block_info(block) {
		const base_x = Math.floor(block.coll.pos.x / 16);
		const high_x = base_x + Math.ceil(block.coll.size.x / 16);
		const base_y = Math.floor(block.coll.pos.y / 16);
		const high_y = base_y + Math.ceil(block.coll.size.y / 16);

		const base_z = Math.floor(block.coll.pos.z / 16);
		const top_z = base_z + Math.ceil(block.coll.size.z / 16);

		// these bounds are large, they must be restricted depending
		// on z.
		let min_scr_y = base_y - top_z;
		let max_scr_y = high_y - base_z;

		let quad_type = "GROUND";
		switch (block.coll.heightShape) {
		case ig.COLL_HEIGHT_SHAPE.NONE:
			max_scr_y = high_y - top_z;
			break;
		case ig.COLL_HEIGHT_SHAPE.WEST_UP:
			quad_type = "SLOPE_WEST";
			break;
		case ig.COLL_HEIGHT_SHAPE.EAST_UP:
			quad_type = "SLOPE_EAST";
			break;
		case ig.COLL_HEIGHT_SHAPE.NORTH_UP:
			quad_type = "SLOPE_NORTH";
			break;
		}
		return { base_x, high_x, base_y, high_y, base_z, top_z,
			 min_scr_y, max_scr_y, quad_type };
	}

	static clip_hidden_block(block_info, your_scr_x, your_scr_y) {

		if (!(block_info.min_scr_y <= your_scr_y
		      && your_scr_y < block_info.max_scr_y))
			return null;

		const { base_x, high_x, base_y, high_y, base_z, top_z,
			quad_type } = block_info;

		let map_z = top_z;
		let tile_z = top_z;
		let { min_scr_y, max_scr_y } = block_info;

		switch (quad_type) {
		case "GROUND":
			break;
		case "SLOPE_WEST":
			map_z -= your_scr_x - base_x + 1;
			// the tile is actually above
			tile_z = map_z + 1;
			min_scr_y = base_y - tile_z;
			max_scr_y = high_y - tile_z;

			break;
		case "SLOPE_EAST":
			map_z -= high_x - your_scr_x;
			// the tile is actually above
			tile_z = map_z + 1;
			min_scr_y = base_y - tile_z;
			max_scr_y = high_y - tile_z;
			break;
		case "SLOPE_NORTH":
			// map_y - map_z = scr_y
			// map_y + map_z = max = top_z + base_y
			// => 2 map_y = top_z + base_y + scr_y
			// => 2 map_z = top_z + base_y - scr_y
			// does not make sense to divide by two ? except it does

			tile_z -= Math.floor((your_scr_y - min_scr_y) / 2);
			map_z = tile_z - 1;

			if (map_z < base_z)
				return null;
			break;
		}

		if (!(min_scr_y <= your_scr_y && your_scr_y < max_scr_y))
			return null;

		return {map_z, block: block_info};
	}

	get_heightinfo(map_x, map_y) {
		const line = this.height_map[map_y + this.height_map_shift];
		if (!line)
			return null;
		return line[map_x] || null;
	}

	handle_tiles_build_map(tiles, left, south) {
		if (tiles.length === 0)
			return;

		const shift_to_z = (tile, wanted_map_z) => {
			const shift = wanted_map_z - tile.map_z;
			tile.map_y += shift;
			tile.map_z = wanted_map_z;
		};
		if (south)
			south = Array.from(south);
		else
			south = [];

		// cleanse the south
		south = south.filter(south_tile => {

			// hidden block or decorative tile
			if (south_tile.tile === undefined
			    || !south_tile.true_tile)
				return false;
			const { type } = south_tile;
			const z_action = MoreTileInfos.z_action_on_tile(type);
			if (z_action === "fall_north")
				// this does not include any information
				return false;
			if (z_action === "ignore")
				return false;
			// boo ! hack.
			south_tile.z_action = z_action;
			return true;
		});


		//left = left ? Array.from(left) || [];

		let current_block = null;
		let last_tile = null;

		const guess_from_block = () => {
			if (current_block !== null) {
				return { z_action: "keepz_north",
					 from_z: current_block.map_z,
					 certitude: 20 };
			}
			return { certitude: 0 };
		};
		const guess_from_south = (my_tile) => {
			let ret = { certitude: 0 };
			let certain = 0;
			let southindex = south.length;
			while (southindex --> 0) {
				const { map_z, z_action } = south[southindex];
				switch (z_action) {
				case "keepz_north":
					// don't keep below
					if (map_z < my_tile.map_z)
						continue;
					// it's a ground.
					if (map_z === my_tile.map_z)
						certain = 10;
					else
						certain = 5;
					break;
				case "rise": // rise on top of south tile
				case "rise_north": // rise next to it (slope)
					// don't rise below
					if (map_z < my_tile.map_z)
						continue;
					certain = 5;
					if (last_tile !== null &&
					    last_tile.map_z < map_z + 1)
						// last_tile drawn above
						// us is below us ?
						// this... is nonsense
						// (autumn/path4 50x47)
						// delete the tile.
						return { certitude: -1 };
					break;
				}
				if (certain > ret.certitude) {
					ret.certitude = certain;
					ret.z_action = z_action;
					ret.index = southindex;
					ret.from_z = map_z;
				} else if (certain < 0)
					return { certitude: -1 };
			}

			if (ret.index === undefined)
				south.length = 0;
			else
				south.length = ret.index;
			return ret;
		};

		// OK, so on our left, we have "tiles" which is the collection
		// of tiles to be drawn at our position, for different heigts.
		// and on our right, we have "south", which is the collection
		// of tiles that were drawn on our south.
		// And we have to match them. And this sucks.

		forEachBackward(tiles, my_tile => {
			if (my_tile.block) {
				current_block = my_tile;
				return;
			}

			// first hint: hidden blocks
			const block_guess = guess_from_block();
			// second hint: south tile
			const south_guess = guess_from_south(my_tile);

			if (block_guess.certitude < 0
			    || south_guess.certitude < 0)
				// the tile is crap, forget it
				return;

			let best_guess = null;
			if (block_guess.certitude > south_guess.certitude)
				best_guess = block_guess;
			else
				best_guess = south_guess;

			switch (best_guess.z_action) {
			case "keepz_north":
				shift_to_z(my_tile, best_guess.from_z);
				break;
			case "rise":
				shift_to_z(my_tile, best_guess.from_z + 1);
				break;
			case "rise_north":
				// this is a hack, i'm drawing twice
				// at the same location
				shift_to_z(my_tile, best_guess.from_z + 1);
				--my_tile.map_z;
			}

			if (last_tile === null) {
				// fill the height map too.
				const my_heightinfo
					= this.get_heightinfo(my_tile.map_x,
							      my_tile.map_y);
				console.assert(my_heightinfo, "i'm nowhere");
				my_heightinfo.z = my_tile.map_z;
				my_heightinfo.type = my_tile.type;
				my_heightinfo.tile = my_tile.tile;
			}
			last_tile = my_tile;
		});

		// now cleanse the non-tiles from the tiles... in place
		// (the above loop could do it ...)
		let i = 0;
		while (i < tiles.length)
			if (tiles[i].tile === undefined)
				tiles.splice(i, 1);
			else
				++i;

	}

	base_maps_by_z(levels, maxlevels) {
		const ret = [];

		for (let levelno = 0; levelno < maxlevels; ++levelno) {
			const level = levels[levelno];

			const maps = [];
			for (const map of level.maps) {
				const distance = Number(map.distance);
				if (distance !== 1)
					continue;
				if (!map.tiles.data)
					continue;
				maps.push(map);
			}

			if (!maps.length)
				continue;

			const next_level = levels[levelno + 1];

			const z_min = level.height;
			const z_max = next_level ? next_level.height : 9999999;

			const mapinfo = this.get_mapinfo(maps[0], z_min, z_max);
			mapinfo.maps = maps;
			ret.push(mapinfo);
			// TODO: detect object maps here and store them
		}
		return ret;
	}


	make_draw_map(base_maps) {
		if (!base_maps.length)
			return null;
		//const absolute_map_z_min = base_maps[0].min_map_z;

		const find_tiles = (scr_x, scr_y, hiddenblocks) => {

			// a hidden block is interesting if we are on top of
			// it, nothing else (for now)
			const blocks = [];
			for (const block_info of hiddenblocks) {
				const clipped
					= BobMap.clip_hidden_block(block_info,
								   scr_x,
								   scr_y);
				if (clipped)
					blocks.push(clipped);
			}

			const tiles = [];
			for (const map_info of base_maps) {
				const { min_map_z, tileinfo } = map_info;
				const map_y = scr_y + min_map_z;

				let tile = map_info.maps[0].data[scr_y][scr_x];
				let true_tile = true;
				let type = "GROUND";
				if (tile === 0) {
					true_tile = false;
					for (let map of map_info.maps) {
						tile = map.data[scr_y][scr_x];
						if (tile !== 0)
							break;
					}
				} else
					type = tileinfo.get_type(tile - 1);

				if (tile === 0)
					continue;

				const tile_pos = {
					map_x: scr_x, map_y, map_z: min_map_z,
					tile: tile - 1,
					true_tile,
					type,
					tileinfo,
					map_info
				};
				tiles.push(tile_pos);
			}
			const all = blocks.concat(tiles);
			// sort by ascending z (even if we iterate it backward
			// later ...)
			// and put hidden blocks before the rest, because the
			// reconstruction algo needs that.
			sortWithKey(all, i => i.map_z + (i.block ? 0.5 : 0));
			return all;
		};

		const draw_map = [];
		for (let scr_x = 0; scr_x < this.mapWidth; ++scr_x) {
			const hidden_block_col = this.hiddenblocks.filter(b => (
				b.base_x <= scr_x && scr_x < b.high_x
			));
			const draw_col = new Array(this.mapHeight);
			draw_map.push(draw_col);

			for (let scr_y = this.mapHeight; scr_y --> 0;) {
				const left = scr_x ? draw_map[scr_x-1][scr_y]
						   : null;
				const south = draw_col[scr_y + 1];
				const tiles = find_tiles(scr_x, scr_y,
							 hidden_block_col);
				draw_col[scr_y] = tiles;
				this.handle_tiles_build_map(tiles, left, south);
			}
		}
		return draw_map;
	}

	handle_tile(result_vector, tileno, draw_info, map_info) {
		if (tileno === -1)
			return;
		const { x, y, z, quad_type } = draw_info;
		const { tilesize } = map_info;

		// The Z Fighting War has begun.
		// Only the strongest will survive.
		// Which mean, not you. Sorry, map.
		// but entities are stronger than you and must be on top of you.
		const draw_z = z - 0.5;

		const quad_vertex
			= BobGeo.make_quad_vertex(x, y + tilesize, draw_z,
						  quad_type,
						  tilesize, tilesize);
		const quad_tex = BobMap.quad_tex(tileno, map_info);

		BobGeo.interleave_triangles(result_vector, quad_type,
					    quad_vertex, quad_tex);
	}

	get_mapinfo(map, z_min, z_max) {
		const tilesize = map.tilesize;
		const width = map.tiles.width;
		const height = map.tiles.height;
		// those are floats, if width isn't multiple of tilesize
		const tiles_on_line = width / tilesize;
		const tiles_on_col = height / tilesize;
		// this one is int, used to match tile numbers to tiles
		const tiles_per_line = Math.floor(tiles_on_line);
		const path = map.tiles.path;
		const tileinfo = this.moretileinfo.get(map.tiles.path);
		const min_map_z = Math.round(z_min / tilesize);
		//const max_map_z = Math.round(z_max / tilesize);

		// z_min and z_max are only theoritic.
		return {
			path,
			tilesize, width, height, tileinfo,
			z_min, z_max,
			tiles_on_line, tiles_on_col, tiles_per_line,
			min_map_z,
		};
	}

	draw_one_map(result_vector, map, draw_map, level) {
		const tile_has_level
			= tile_pos => tile_pos.map_info.z_min === level;
		const get_tile = tiles => tiles.find(tile_has_level);

		const handle_tile = this.handle_tile.bind(this, result_vector);
		const map_info = this.get_mapinfo(map, level, 42);
		const tilesize = map_info.tilesize;

		let count = 0;
		draw_map.forEach((draw_col, scr_x) => {
			draw_col.forEach((draw_info, scr_y) => {
				const tile = map.data[scr_y][scr_x] - 1;
				if (tile === -1)
					return;
				const tile_pos = get_tile(draw_info);
				const real_draw_info = {
					x: tile_pos.map_x * tilesize,
					y: tile_pos.map_y * tilesize,
					z: tile_pos.map_z * tilesize,
					quad_type: tile_pos.type
				};
				handle_tile(tile, real_draw_info, map_info);
				++count;
			});
		});

		return count * 6; // we add two triangles of 3 vertex each time
	}

	draw_walls(result_vector, map_info) {

		const { min_z_map, tilesize } = map_info;
		// FIXME: need most negative height
		const z_or_zmin = z => z !== null ? z : -4;

		let count = 0;

		const make_wall = (map_x, map_y, from_z, to_z, wall_type,
				   start_tile, wall_tile) => {
			let quad_type = "BOTTOM_" + wall_type;
			let tileno = start_tile;
			if (tileno === null)
				tileno = wall_tile;

			for (let map_z = from_z; map_z < to_z; ++map_z) {
				const draw_info = {
					x: map_x * tilesize,
					y: map_y * tilesize,
					z: map_z * tilesize,
					quad_type: quad_type
				};
				if (tileno === null)
					return;
				this.handle_tile(result_vector, tileno,
						 draw_info, map_info);
				++count;
				quad_type = wall_type;
				tileno = wall_tile;
			}
		};

		// BORDER_SW/SE are already visible, don't draw them
		const wall_types = {
			RISE_BORDER_WEST: "WALL_EAST",
			FALL_BORDER_EAST: "WALL_WEST",
			RISE_BORDER_NW: "WALL_SE",
			FALL_BORDER_NE: "WALL_SW",

			RISE_BORDER_NW_FLAT: "WALL_EAST",
			RISE_BORDER_SW_FLAT: "WALL_EAST",

			FALL_BORDER_NE_FLAT: "WALL_WEST",
			FALL_BORDER_SE_FLAT: "WALL_WEST"
		};
		// use this tile for the given wall type
		const tile_types = {
			WALL_EAST: "WALL_NORTH",
			WALL_WEST: "WALL_NORTH",
			WALL_SE: "WALL_NW",
			WALL_SW: "WALL_NE"
		};

		forEachBackward(this.height_map, (line, shifted_map_y) => {
			const map_y = shifted_map_y - this.height_map_shift;
			let left = null;
			let next_left = null;
			line.forEach((heightinfo, map_x) => {
				left = next_left;
				next_left = heightinfo;
				if (left === null)
					return;

				const my_z = z_or_zmin(heightinfo.z);
				const left_z = z_or_zmin(left.z);
				if (my_z === left_z)
					return;
				let key;
				let from_z, to_z;
				let tile;
				if (my_z > left_z) {
					key = "RISE_" + heightinfo.type;
					from_z = left_z;
					to_z = my_z;
					tile = heightinfo.tile;
				} else {
					key = "FALL_" + left.type;
					from_z = my_z;
					to_z = left_z;
					--map_x;
					tile = left.tile;
				}
				if (to_z < min_z_map)
					return; // lower level already did it
				const wall_type = wall_types[key];
				if (!wall_type)
					return;

				const tile_type = tile_types[wall_type];

				const wall_tile = (
					map_info.tileinfo.get_default
						.bind(map_info.tileinfo, tile)
				);

				make_wall(map_x, map_y, from_z, to_z,
					  wall_type,
					  wall_tile("BOTTOM_"+tile_type),
					  wall_tile(tile_type));
			});
		});

		return count * 6;
	}
	steal_map() {
		// steal the entities.
		for (const entity of ig.game.entities) {
			if (entity instanceof ig.ENTITY.HiddenBlock) {
				const block_info
					= BobMap.hidden_block_info(entity);
				this.hiddenblocks.push(block_info);
			} else if (entity instanceof ig.ENTITY.ObjectLayerView)
				this.layerviews.push(entity);
		}

		// AAHHHH where are my quads ? they take away my display lists,
		// and now they take away my quads too ?
		// i have to do TRIANGLES ? TRIANGLES SUCKS ! QUADROGUARD FTW !
		const everything = [];
		let i = 0;

		// should probably migrate to vertex indexes. at least they
		// kept this.

		const textures_ranges = this.textures_ranges;
		textures_ranges.length = 0;
		// should probably reorder by texture, too ? or nobody cares
		// because it's probable only one is used ?
		const tex_trove = this.texture_trove;

		const base_maps = this.base_maps_by_z(ig.game.levels,
						      ig.game.maxLevel);
		const draw_map = this.make_draw_map(base_maps);

		if (!draw_map) {
			// happens at start
			tex_trove.cleanup();
			fill_const_buffer(this.context, this.buf, everything);
			return;
		}

		// find all required textures

		// really need a map with insertion order
		const texture_obj = { order: []};
		forEachBackward(ig.game.levels, level => {
			for (const map of level.maps) {
				if (map.tiles.path in texture_obj)
					continue;
				const path = map.tiles.path;
				const img = map.tiles.data;
				const texture = tex_trove.add(path, img);
				texture_obj[path] = texture;
				texture_obj.order.push(path);
			}
		}, ig.game.maxLevel - 1);

		const get_info_if_base_map = maybe_base => (
			base_maps.find(map_info => (
				map_info.maps[0] === maybe_base
			))
		);

		// now iterate them

		for (const texture_path of texture_obj.order) {
			const texture = texture_obj[texture_path];
			const text_range = { start: i, texture: texture };
			forEachBackward(ig.game.levels, (level, levelno) => {
				const z_level = level.height;
				for (const map of level.maps) {
					if (map.tiles.path !== texture_path)
						continue;
					if (Number(map.distance) !== 1)
						// TODO: render parallax maps.
						// possibly in 2d.
						continue;
					// i don't think i have written so many
					// imbricated fors in my life.
					// maybe i'm just missing my dear bmi ?
					i += this.draw_one_map(everything, map,
							       draw_map,
							       z_level);
					const map_info
						= get_info_if_base_map(map);
					if (map_info)
						i += this.draw_walls(everything,
								     map_info);
				}
			}, ig.game.maxLevel - 1);

			text_range.size = i - text_range.start;
			if (text_range.size)
				textures_ranges.push(text_range);
		}

		tex_trove.cleanup();

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
	constructor(context, locations_opaque, locations_blended,
		    moretileinfo) {
		super(context, locations_opaque, locations_blended);
		this.moretileinfo = moretileinfo;
	}
	clear() {
		this.sprites_by_texture = {};
		this.blend_sprites_by_z = [];
		this.textures_ranges.length = 0;
	}
	do_overrides(path, cubesprite) {
		// I have LOADS of reserves on how the game classify ground
		// and wall sprites.
		// i will list them ALL HERE ! MUAHAHAHA !
		const splitted = path.split("/");
		const override = walk_break_on_first(path_overrides, splitted);
		if (override)
			return override; // for now.

		const tileinfo = this.moretileinfo.get(path);
		if (tileinfo.found) {
			let src_x = cubesprite.src.x;
			let src_y = cubesprite.src.y;
			if (cubesprite.image instanceof ig.ImagePattern) {
				src_x = cubesprite.image.sourceX;
				src_y = cubesprite.image.sourceY;
			}
			const tiles_per_line = 512 / 16;
			const tile_x = Math.floor(src_x / 16);
			const tile_y = Math.floor(src_y / 16);
			const tileno = tile_x + tile_y * tiles_per_line;
			const quad_type = tileinfo.get_type(tileno);
			if (quad_type.indexOf("WALL") !== -1)
				// FIXME: we could return more, and waterfallz
				// need it.
				return "wall";
		}

		if (!cubesprite.size.z)
			return "ground";
		return null;
	}
	prepare_sprites(spritearray) {
		if (ig.system.context.globalAlpha !== 1)
			console.log("global alpha not one !");
		const tex_trove = this.texture_trove;
		const by_texture = this.sprites_by_texture;
		for (const sprite of spritearray) {
			const cs = sprite.cubeSprite;

			let path;
			let image;
			if (cs.image instanceof ig.ImagePattern) {
				path = cs.image.sourceImage.path;
				image = cs.image.sourceImage.data;
			} else if (cs.image && cs.image.path && cs.image.data) {
				path = cs.image.path;
				image = cs.image.data;
			} else if (!(cs.image instanceof ig.ImageCanvasWrapper))
				// FIXME: ImageCanvasWrapper will be hard to
				// cache... but it's the majority of sprites ?
				console.log("strange sprite");
			if (!path)
				continue;

			let has_opaque = true;
			let has_blending = false;
			if (cs.renderMode
			    || Number(cs.alpha) !== 1
			    || cs.overlay.color) {
				has_opaque = false;
				has_blending = true;
			}
			if (cs.lighterOverlay.color)
				has_blending = true;

			if (has_blending)
				this.blend_sprites_by_z.push(
					{ sprite, path });

			if (!has_opaque) {
				tex_trove.add(path, image);
				continue;
			}


			let textureinfo = by_texture[path];
			if (!textureinfo) {
				const texture = tex_trove.add(path, image);
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
	//
	// we don't do flips here, the vertexes are supposed to do it.
	// (but that's is infuriating ?)
	static get_2d_src_crop(cubesprite, ground) {
		const cs = cubesprite;
		let offsetx = cs.gfxCut.left;
		let offsety = 0;
		let sizex
			= cs.size.x - cs.gfxCut.right - cs.gfxCut.left;

		if (cs.flip.x)
			// basically, gfxCut.left and gfxCut.right
			// happens after flipping.
			// basically, we should swap them
			offsetx = cs.gfxCut.right;

		let sizey = cs.size.y + cs.size.z;

		// yes, there are wall sprites with cs.wallY = cs.size.y.
		// Even if i think they should really be ground sprites.
		// (this includes sweeps)

		let cuttop = 0;
		let cutbottom = 0;
		if (cs.wallY >= cs.size.y)
			// apply default
			;
		else if (ground)
			// keep ground but amputate wallY from it
			cutbottom = sizey - (cs.size.y - cs.wallY);
		else if (!cs.mergeTop) {
			// remove the ground part
			cuttop = cs.size.y - cs.wallY;
		}
		cuttop = Math.max(cs.gfxCut.top, cuttop);
		cutbottom = Math.max(cs.gfxCut.bottom, cutbottom);
		offsety = cuttop;
		sizey = sizey - cuttop - cutbottom;

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
		// do the actual flips, by inverting the st coords
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
	// return number of vertex added.
	// merely calculates the sprite vertex from the src_quad_tex stuff
	handle_basic_sprite(result_vector, pos, quad_type, src_quad_tex, cs) {
		// normal sprite is normal.
		const make_quad = BobGeo.make_rotated_quad_vertex.bind(BobGeo);

		let quad_vertex = make_quad(pos, quad_type,
					    [src_quad_tex.sizex,
					    src_quad_tex.sizey],
					    [cs.pivot.x || 0, cs.pivot.y || 0],
					    cs.rotate || 0,
					    [cs.scale.x, cs.scale.y]);

		BobGeo.interleave_triangles(result_vector, quad_type,
					    quad_vertex,
					    src_quad_tex.quad_tex);
		return 6;
	}
	// blit a tile repeatedly.
	// (some smartass could say "well, let the graphic card do it !"
	//  and they would probably be right)
	handle_image_pattern(result_vector, pos, quad_type, src_quad_tex, cs) {
		const image = cs.image;
		// handle the dreaded image pattern

		// TODO: should rename pivot_shift to "origin"
		// (that's (0,0) rotated around the pivot)
		const { pivot_shift, rotate_mat }
			= BobGeo.make_rotated_quad_matrix(cs.rotate || 0,
							  [cs.scale.x,
							   cs.scale.y],
							  [cs.pivot.x || 0,
							   cs.pivot.y || 0]);
		const origin = pivot_shift;
		const x_vector = len => mulscalar(getcol(rotate_mat, 0), len);
		const y_vector = len => mulscalar(getcol(rotate_mat, 1), len);

		const iterate_patch = (initial_offset, total_size, patch_size,
				       cb) => {
			let size;
			for (let i = 0; i < total_size; i+= size) {
				const src_i = (i + initial_offset) % patch_size;
				if (i + patch_size < total_size)
					size = patch_size - src_i;
				else
					size = total_size - i;
				cb(src_i, size, i);
			}
		};

		const make_raw
			= BobGeo.make_quad_raw.bind(BobGeo, ...pos, quad_type,
						    src_quad_tex.sizex,
						    src_quad_tex.sizey);
		const make_raw_tex = BobGeo.make_quad_tex.bind(BobGeo);

		let vertex_count = 0;

		// the pattern to blit around is sourceX,sourceY+width,height
		const blit_it = (src_x, src_y, size_x, size_y, vertexes) => {
			const quad_vertexes = make_raw(vertexes);

			// get the texture coordinate from the source.
			const quad_tex
				= make_raw_tex(image.sourceX + src_x,
					       image.sourceY + src_y,
					       image.sourceImage.data.width,
					       image.sourceImage.data.height,
					       size_x, size_y);

			BobGeo.interleave_triangles(result_vector, quad_type,
						    quad_vertexes, quad_tex);
			vertex_count += 6;
		};

		// note: the src_quad_tex.quad_tex is total crap here.
		const initial_offsetx
			= (src_quad_tex.offsetx + cs.src.x) % image.width;
		const initial_offsety
			= (src_quad_tex.offsety + cs.src.y) % image.height;

		iterate_patch(initial_offsetx, src_quad_tex.sizex,
			      image.width, (src_x, src_size_x, dest_x) => {

			const top_y_left = addvec(origin, x_vector(dest_x));
			const top_y_right = addvec(top_y_left,
						   x_vector(src_size_x));

			iterate_patch(initial_offsety, src_quad_tex.sizey,
				      image.height,
				      (src_y, src_size_y, dest_y) => {
				const top_y_to_top = y_vector(dest_y);
				const topleft
					= addvec(top_y_left, top_y_to_top);
				const topright = addvec(top_y_right,
							 top_y_to_top);
				const top_to_bottom = y_vector(src_size_y);
				const bottomleft
					= addvec(topleft, top_to_bottom);
				const bottomright
					= addvec(topright, top_to_bottom);

				blit_it(src_x, src_y, src_size_x, src_size_y,
					{ topleft, topright, bottomleft,
					  bottomright });
			});

		});
		return vertex_count;
	}
	handle_one_sprite(result_vector, path, sprite) {
		const cs = sprite.cubeSprite;
		let is_ground = sprite.ground;
		// i have some reserves on how the game classify ground
		// sprites from wall sprites.
		// FIXME: this affects ground removal, is that intended ?
		switch (this.do_overrides(path, cs)) {
			case "ground":
				is_ground = true;
				break;
			case "wall":
				is_ground = false;
				break;
		}

		const src_quad_tex
			= BobEntities.get_src_quad_tex(cs, is_ground);

		if (!src_quad_tex)
			return 0;

		// BobGeo want low x, high y, low z
		let x = cs.pos.x + cs.tmpOffset.x + cs.gfxOffset.x +
			cs.gfxCut.left;
		let y = (cs.pos.y + cs.tmpOffset.y + cs.size.y
				  + cs.gfxOffset.y);
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
		const image = cs.image;
		if (image && image.path && image.data)
			return this.handle_basic_sprite(result_vector,
							[x, y, z], quad_type,
							src_quad_tex, cs);
		if (image instanceof ig.ImagePattern)
			return this.handle_image_pattern(result_vector,
							 [x, y, z], quad_type,
							 src_quad_tex, cs);

		console.assert(false, "not supported, how did you get there ?");
		return 0;
	}
	finalize_opaque_sprites(result_vector, i) {
		for (const path in this.sprites_by_texture) {
			const texture = this.sprites_by_texture[path];
			const start_i = i;

			for (const sprite of texture.sprites)
				i += this.handle_one_sprite(result_vector,
							    path,
							    sprite);

			if (i === start_i)
				continue;

			this.textures_ranges.push({
				texture: texture.texture,
				start: start_i,
				size: i - start_i
			});
		}
		return i;
	}
	finalize_blending_sprites(result_vector, i) {
		const get_color = color_calculator.get.bind(color_calculator);
		this.blending_ranges.length = 0;
		for (const blend_sprite of this.blend_sprites_by_z) {
			const start = i;
			const path = blend_sprite.path;
			const cs = blend_sprite.sprite.cubeSprite;
			i += this.handle_one_sprite(result_vector,
						    path,
						    blend_sprite.sprite);
			const size = i - start;
			if (!size)
				continue;

			const texture = this.texture_trove.get(path);
			const add = (blendmode, alpha,
				     color, color_alpha) => {
				if (!color)
					color = [0,0,0,0];
				else
					color[3] = color_alpha;
				this.blending_ranges.push({
					start, size, texture,
					mode: blendmode, alpha,
					blend_color: color
				});
			};

			// apparently, there cannot be image patterns here ?
			// some idiots put strings here.
			const alpha_base = Number(cs.alpha);

			// FIXME: the game is idiotic enough to have
			// both overlays and lighter overlay.
			// we should precalculate the color and stuff it.

			if (!cs.overlay.color) {
				if (cs.renderMode === "lighter")
					add("lighter", alpha_base, null, 0);
				else if (!cs.renderMode
					 || cs.renderMode === "source-over")
					add("normal", alpha_base, null, 0);
				else
					console.warn("sprite with renderMode",
						     cs.renderMode);
			} else {
				const color = get_color(cs.overlay.color);
				add("normal", alpha_base,
				    color, Number(cs.overlay.alpha));
			}
			const lighter_color = cs.lighterOverlay.color;
			if (lighter_color) {
				const color = get_color(lighter_color);
				add("lighter", alpha_base,
				    color, Number(cs.lighterOverlay.alpha));
			}
		}
		return i;
	}
	finalize() {
		const everything = [];
		let i = 0;
		i = this.finalize_opaque_sprites(everything, i);
		this.finalize_blending_sprites(everything, i);

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

	setup_canvas(canvas, moretileinfo) {

		const ratio = canvas.width / canvas.height;
		// need a real reflection on fov:
		// - don't want rotate - vertical fov/2 to be below 0, otherwise
		// we will have to render the back of cubes
		// - don't want rotate + vertical fov/2 to be above 90°,
		// otherwise there will be black backgrounds to render.
		// but that's the vertical fov, which needs to be calculated
		// from the horizontal fov, which is probably
		// vfov = 2 * atan(ratio * tan(hfov/2))
		const fov = Math.PI * 0.4;
		this.proj_matrix = throw_at_wall(fov, ratio, -100, -700);

		this.context = webglplz(canvas);

		this.vertexshader = compile_shader(this.context,
						   "VERTEX_SHADER", `
		attribute vec4 pos;
		attribute vec2 texcoord;
		uniform mat4 projectmat;
		varying mediump vec2 texcoord2;
		void main() {
			gl_Position = projectmat * pos;
			texcoord2 = texcoord;
		}
		`);

		this.fragshader = compile_shader(this.context,
						 "FRAGMENT_SHADER", `
		varying mediump vec2 texcoord2;
		uniform sampler2D colorsampler;
		void main() {
			// FIXME: find what control interpolation in there.
			gl_FragColor = texture2D(colorsampler, texcoord2);
			if (gl_FragColor.a == 0.)
				discard;
			// gl_FragColor = vec4(1, /*gl_Position.x*/ 0, 1, 1);
		}
		`);
		this.blendung_shader = compile_shader(this.context,
						      "FRAGMENT_SHADER", `

		varying mediump vec2 texcoord2;
		uniform sampler2D colorsampler;
		uniform mediump vec4 blend_color;

		void main() {
			mediump vec4 color = texture2D(colorsampler, texcoord2);
			if (color.a == 0.)
				discard;
			mediump vec3 blended = mix(color.rgb, blend_color.rgb,
						   blend_color.a);

			gl_FragColor = vec4(blended, 1);
		}
		`);

		this.program = create_program(this.context,
					      [this.vertexshader,
					       this.fragshader]);
		this.locations = extractlocations(this.context, this.program,
						  ["pos", "texcoord"],
						  ["projectmat",
						   "colorsampler"]);
		this.blend_program = create_program(this.context,
						    [this.vertexshader,
						     this.blendung_shader]);
		this.blend_locations = extractlocations(this.context,
							this.blend_program,
							["pos", "texcoord"],
							["projectmat",
							 "colorsampler",
							 "blend_color"]);

		this.context.useProgram(this.program);

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

		// maybe disable dithering ?
		this.context.disable(this.context.DITHER);

		const opaque_locations = {
			pos: this.locations.pos,
			tex_coord: this.locations.texcoord
		};
		const blend_locations = {
			pos: this.blend_locations.pos,
			tex_coord: this.blend_locations.texcoord,
			color_blend: this.blend_locations.blend_color
		};

		this.map = new BobMap(this.context,
				      opaque_locations,
				      blend_locations,
				      moretileinfo);
		this.entities = new BobEntities(this.context,
						opaque_locations,
						blend_locations,
						moretileinfo);
	}

	set_uniforms(uniforms) {
		stuff_matrix_to_uniform(this.context,
					uniforms.projectmat,
					this.matrix_all);
		// Assume that TEXTURE0 is the base color texture
		this.context.uniform1i(uniforms.colorsampler, 0);
		// note: TEXTURE0 is the default ACTIVE_TEXTURE.
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

		this.context.useProgram(this.program);
		disable_blending(this.context);

		this.set_uniforms(this.locations);

		this.entities.clear();
		this.entities.prepare_sprites(ig.game.renderer.spriteSlots);
		this.entities.prepare_sprites(ig.game.renderer.guiSpriteSlots);
		this.entities.finalize();
		this.map.render_opaque();
		this.entities.render_opaque();

		// START ALPHA BLENDONG
		this.context.useProgram(this.blend_program);
		enable_blending(this.context);
		this.set_uniforms(this.blend_locations);

		this.entities.render_blended();
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
				onLevelLoadStart: function(map) {
					// map comes directly from json
					// tilesets not loaded
					me.map.steal_map_data(map);
				},
				onLevelLoaded: function() {
					// tilesets are loaded now
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

		//const centerx = ig.camera._currentZoomPos.x;
		//let centery = ig.camera._currentZoomPos.y;
		const centerx = ig.camera._currentPos.x;
		let centery = ig.camera._currentPos.y;
		let centerz = 0;
		const targets = ig.camera.targets;
		if (targets.length) {
			const last = targets[targets.length - 1];
			if (last.target && last.target._currentZ) {
				centerz = last.target._currentZ;
				centery += centerz;
			}
		}
		const zoom = ig.camera._currentZoom || 1;
		// move center of screen at (0,0,0)
		translate_matrix_before(view_matrix,
					-centerx + this.debugshift.x,
					-centery + this.debugshift.y,
					-centerz + this.debugshift.z);
		// take the camera back by 300
		translate_matrix(view_matrix,
				 0,
				 0,
				 -275 / zoom);

		this.matrix_all = mulmat(this.proj_matrix, view_matrix);

		this.context.clear(this.context.COLOR_BUFFER_BIT
				   | this.context.DEPTH_BUFFER_BIT);
	}
}

class MoreTileInfos {
	static parse_entry(key, value) {

		const res = /^(\d+),(\d+)(?:\+(\d+),(\d+))?$/.exec(key);
		if (!res) {
			console.assert(false, "invalid entry:", key);
			return null;
		}

		let x = Math.round(Number.parseInt(res[1]) / 16);
		let y = Math.round(Number.parseInt(res[2]) / 16);
		let size_x = 1, size_y = 1;
		if (res[3]) {
			size_x = Math.round(Number.parseInt(res[3]) / 16);
			size_y = Math.round(Number.parseInt(res[4]) / 16);
		}

		let info;
		if (value.constructor === String)
			info = value.split(/\s+/).filter(s => s);
		else if (Array.isArray(value))
			info = value;
		else
			throw "insupported stuff: "+ info;
		return { x, y, size_x, size_y, info };
	}
	static handle_type1(size_x, size_y, add_function) {
		console.assert(size_x === 6);
		console.assert(size_y >= 4);
		const TYPE1_BASE_INFO = [
			null, "BORDER_NW", "BORDER_NORTH",
			"BORDER_NORTH", "BORDER_NE", null,

			"BORDER_NW", "GROUND", "BORDER_WEST",
			"BORDER_EAST", "GROUND", "BORDER_NE",

			"BORDER_SW", "GROUND", "BORDER_WEST",
			"BORDER_EAST", "GROUND", "BORDER_SE",

			null, "BORDER_SW", "BORDER_SOUTH",
			"BORDER_SOUTH", "BORDER_SE", null
		];
		TYPE1_BASE_INFO.forEach((type, i) => {
			if (!type)
				return;
			add_function(i % 6, Math.floor(i / 6), type);
		});

		const TYPE1_WALLS = [
			"WALL_NE", "WALL_NE", "WALL_NORTH",
			"WALL_NORTH", "WALL_NW", "WALL_NW"
		];

		for (let chipy = 4; chipy < size_y; ++chipy) {
			const prefix = chipy === size_y - 1 ? "BOTTOM_" : "";
			TYPE1_WALLS.forEach((type, sx) => {
				add_function(sx,
					     chipy - Number(sx % 5 === 0),
					     prefix + type);
			});
		}
	}
	static handle_type2(size_x, size_y, add_function) {
		console.assert(size_x === 5);
		console.assert(size_y >= 4);

		const TYPE2_BASE_INFO = [
			"BORDER_NW", "BORDER_NORTH", "BORDER_NE",
			"BORDER_NW_FLAT", "BORDER_NE_FLAT",

			"GROUND", "GROUND", "GROUND",
			"BORDER_WEST", "BORDER_EAST",

			"GROUND", "GROUND", "GROUND",
			"BORDER_SW_FLAT", "BORDER_SE_FLAT",

			"BORDER_SW", "BORDER_SOUTH", "BORDER_SE"
		];
		TYPE2_BASE_INFO.forEach((type, i) => {
			add_function(i % 5, Math.floor(i / 5), type);
		});

		const TYPE2_WALLS = [
			"WALL_NE", "WALL_NORTH", "WALL_SE",
			"WALL_NORTH", "WALL_NORTH"
		];

		for (let i = TYPE2_BASE_INFO.length; i < size_y * size_x; ++i) {
			const typeno = i % 5;
			const sy = Math.floor(i / 5);

			let type = TYPE2_WALLS[typeno];
			const bottom
				= sy === size_y - 1 ? "BOTTOM_" : "";
			add_function(typeno, sy, bottom + type);
		}
	}
	static interpret_entry(entry, by_tile, defaults) {
		const { info, x, y, size_x, size_y } = entry;
		let is_default = false;
		{
			const default_i = info.indexOf("DEFAULT");
			if (default_i !== -1) {
				is_default = true;
				info.splice(default_i, 1);
			}
		}
		const add_shift = (sx, sy, type) => {
			const tileno = x + sx + (y + sy) * 512 / 16;
			by_tile[tileno] = { type: type, is_default };
			if (is_default && defaults[type] === undefined)
				defaults[type] = tileno;
		};
		console.assert(info.length === 1, "unsupported stuff", info);

		switch (info[0]) {
		case "TYPE1":
			this.handle_type1(size_x, size_y, add_shift);
			break;
		case "TYPE2":
			this.handle_type2(size_x, size_y, add_shift);
			break;
		default:
			for (let sx = 0; sx < size_x; ++sx)
				for (let sy = 0; sy < size_y; ++sy)
					add_shift(sx, sy, info[0]);
		}
	}
	static parse_tileset(data) {
		const entries = [];
		for (let key in data) {
			const entry = this.parse_entry(key, data[key]);
			if (entry !== null)
				entries.push(entry);
		}
		sortWithKey(entries, entry => -entry.size_x * entry.size_y);
		const defaults = {};
		const by_tileno = {};
		entries.forEach(entry => this.interpret_entry(entry, by_tileno,
							      defaults));
		return { by_tileno, defaults };
	}
	constructor(data) {
		this.tileinfo = {};
		for (const tilepath in data) {
			const set_by_tileno = {};
			const sets = {};
			for (const setname in data[tilepath]) {
				const set = data[tilepath][setname];
				const set_info
					= MoreTileInfos.parse_tileset(set);

				sets[setname] = set_info;
				for (const tileno in set_info.by_tileno)
					set_by_tileno[tileno] = set_info;
			}
			const info = { by_tileno: set_by_tileno, sets,
				       found: true};

			info.get_type
				= MoreTileInfos.get_type.bind(null, info);

			info.get_default
				= MoreTileInfos.get_default.bind(null, info);
			this.tileinfo[tilepath] = info;
		}
	}
	static get_type(set_info, tileno) {
		const set = set_info.by_tileno[tileno];
		if (!set)
			return "GROUND";
		return set.by_tileno[tileno].type;
	}
	static get_default(set_info, tileno, type) {
		const set = set_info.by_tileno[tileno];
		if (!set)
			return null;
		return set.defaults[type] || null;
	}
	get(path) {
		path = path.replace(/.*[/]/, "");
		let ret = this.tileinfo[path];
		if (!ret) {
			ret = { get_type: () => "BORDER_NORTH",
				get_default: () => null,
				found: false };
		}
		return ret;
	}

	// what to do on the tile on top of this one
	static z_action_on_tile(tileinfo) {
		if (tileinfo.startsWith("IGNORE_"))
			return "ignore"; // decorative in first map
		switch (tileinfo) {
		case "BORDER_SOUTH":
		case "BORDER_SW":
		case "BORDER_SE":
		case "BORDER_SW_FLAT":
		case "BORDER_SE_FLAT":
		case "BORDER_EAST":
		case "BORDER_WEST":
		case "SLOPE_WEST":
		case "SLOPE_EAST":
		case "GROUND":
			return "keepz_north"; // keep z and go north
		case "WALL_NORTH":
		case "WALL_NE":
		case "WALL_NW":
		case "BOTTOM_WALL_NORTH":
		case "BOTTOM_WALL_NE":
		case "BOTTOM_WALL_NW":
		case "SLOPE_WEST_WALL_NE":
		case "SLOPE_EAST_WALL_NW":
			return "rise"; // rise on top of this one
		case "SLOPE_NORTH":
			return "rise_north"; // rise and go north
		case "BORDER_NORTH":
		case "BORDER_NE":
		case "BORDER_NW":
		case "BORDER_NE_FLAT":
		case "BORDER_NW_FLAT":
		case "SLOPE_WEST_BORDER_NE": // these are borders, above all
		case "SLOPE_EAST_BORDER_NW":
			return "fall_north"; // fall and go north
		default:
			throw "unknown tileinfo" + tileinfo;
		}
	}
}

export default class Mod extends Plugin {
	constructor(what) {
		super(what);
	}
	load_moreinfo () {
		return new Promise((resolve, reject) => {
			$.ajax({
				dataType:"json",
				url:"assets/data/more-tile-infos.json",
				success: d => resolve(new MoreTileInfos(d)),
				error: reject
			});
		});
	}
	preload() {
	}
	postload() {

		this.renderer = new BobRender();
		this.renderer.bind_to_game();
	}
	async main() {
		// debug
		ig.system.canvas.style.margin = "0px";

		this.canvas3d = document.createElement("canvas");
		this.canvas3d.width = 570;
		this.canvas3d.height = 320;
		this.canvas3d.style.marginTop = "320px";
		document.getElementById("game").appendChild(this.canvas3d);

		const moreinfo = await this.load_moreinfo();
		this.renderer.setup_canvas(this.canvas3d, moreinfo);
	}
}
