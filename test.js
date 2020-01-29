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
}

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
		console.assert(ret[attrib] !== undefined);
	}
	for (const global of globals) {
		ret[global] = context.getUniformLocation(program, global);
		console.assert(ret[global] !== undefined);
	}
	return ret;
};

// this assumes data is alread a Float32Array
const create_const_buffer = (context, data) => {
	const buffer = context.createBuffer();
	context.bindBuffer(context.ARRAY_BUFFER, buffer);
	context.bufferData(context.ARRAY_BUFFER, data, context.STATIC_DRAW);
	return buffer;
};

// count is mandatory because the webgl api sucks.
const set_vertexes = (context, vertex_buffer, vertex_location, components,
		      stride, offset) => {
	console.assert(vertex_buffer !== undefined && vertex_location !== undefined);
	context.bindBuffer(context.ARRAY_BUFFER, vertex_buffer);
	context.vertexAttribPointer(vertex_location, components,
				    context.FLOAT,
				    false, stride || 0, offset || 0);
	context.enableVertexAttribArray(vertex_location);
};

const fill_buffer = (context, buffer) => {
	// where are my display lists ?
	context.bindBuffer(context.ARRAY_BUFFER, buffer);
	context.bufferData(context.ARRAY_BUFFER, new Float32Array(data),
			   context.STATIC_DRAW);
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

const mulmat = (mat1, mat2) => mat1.map(
	(line1, line1no) =>
		line1.reduce(
			(resline, val1, col1) =>
				resline.map((resval, col2) =>
					    resval + val1 * mat2[col1][col2]),
			[0, 0, 0, 0])
);

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

const translate_matrix = (matrix, x, y, z) => {
	// because multiplying with a translation matrix is too wasteful
	// we multiply the translation on the left
	// this assumes matrix has [0, 0, 0, 1] as last line (i.e. no proj)
	// [1, 0, 0, x]   [a, b, c, x']   [a, b, c, x + x']
	// [0, 1, 0, y] * [d, e, f, y'] = [d, e, f, y + y']
	// [0, 0, 1, z]   [g, h, j, z']   [
	// [0, 0, 0, 1]   [0, 0, 0, 1 ]   [0, 0, 0, 1]
	matrix[0][3] += x;
	matrix[1][3] += y;
	matrix[2][3] += z;
}

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
	// is ALSO divided by the fourth coordinate.
	//
	// so, zmin must be mapped to a value, which, when divided by zmin,
	//     maps to -1... so zmin must map to -zmin
	// and zmax must be mapped to a value, which, when divided by zmax,
	//     maps to 1... so zmax must map to zmax
	//
	// a stupid affine function. zmax-zmin must be mapped to a difference
	// of zmax - (-zmin), so multiply by (zmax + zmin)/(zmax-zmin)
	const divisor = 1/(zmax-zmin);
	//
	// and zmax must still map to zmax, but now it maps to
	// zmax * (zmax + zmin)/(zmax-zmin), so just add
	// zmax - zmax * (zmax + zmin)/(zmax-zmin) to the result
	// = (zmax² - zmin *zmax - zmax² - zmax*zmin)/(zmax-zmin)
	// = -2 zmin*zmax / (zmax-zmin)
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
	context.uniformMatrix4fv(mat_location, false, flattened)
};

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

const load_me_a_texture = (context, url) => new Promise((resolve, reject) => {
	const img = new Image();
	img.onload = () => resolve(create_texture(context, img));
	img.onerror = reject;
	img.src = url;
});


class MyApp {
	constructor () {

		this.context = webglplz(document.getElementById("hello"));

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
						 'FRAGMENT_SHADER', `
		varying highp vec2 texcoord2;
		uniform sampler2D colorsampler;
		void main() {
			// FIXME: find what control interpolation in there.
			gl_FragColor = texture2D(colorsampler, texcoord2 / 4.);
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

		// this is a triangle strip
		// will probably migrate to quads afterwards.
		// also, couldn't find default value of 4 coordinate, hoping
		// it's 1.
		const vertexes = create_const_buffer(this.context, 
			Float32Array.of(
				// left bottom
				-1,1,  0,
				// right bottom
				1,1,   0,
				// left top
				-1,-1, 0,
				// right top
				1,-1,  0));

		set_vertexes(this.context, vertexes, this.locations.pos, 3);


		const texcoords = create_const_buffer(this.context,
			Float32Array.of(
				// left bottom ... so this assumes screen, ok.
				0, 1,
				// right bottom
				1, 1,
				// left top
				0, 0,
				// right top
				1, 0));

		set_vertexes(this.context, texcoords, this.locations.texcoord,
			     2);


		this.proj_matrix = throw_at_wall(Math.PI/4, 4/3, -1, -200);
		this.context.enable(this.context.DEPTH_TEST);
		// isn't that the default ?
		this.context.depthFunc(this.context.LEQUAL);
		this.context.clearColor(0, 0, 1, 1); // blue sky (ok ...)
		// didn't remember depths to be between -1 and 1 ?
		this.context.clearDepth(1);

		this.nudge_angle = 0;
	}
	async load () {
		this.texture = await load_me_a_texture(this.context,
						       "./image.png");
	}
	render_one() {
		const view_matrix = rotate_me(Math.PI/4, this.nudge_angle, 0.1);
		translate_matrix(view_matrix, 0, 0, -10);

		const mulled = mulmat(this.proj_matrix, view_matrix);
		stuff_matrix_to_uniform(this.context, this.locations.projectmat,
					mulled);

		this.context.clear(this.context.COLOR_BUFFER_BIT
				   | this.context.DEPTH_BUFFER_BIT);

		this.context.activeTexture(this.context.TEXTURE0);
		this.context.bindTexture(this.context.TEXTURE_2D, this.texture);
		this.context.uniform1i(this.locations.colorsampler, 0);

		this.context.drawArrays(this.context.TRIANGLE_STRIP, 0, 4);
	}
}

(async () => {
	const app = new MyApp();
	await app.load();
	function render_me_beautiful(millisec) {
		app.nudge_angle = millisec / 250;
		app.render_one();
		requestAnimationFrame(render_me_beautiful);
	}

	render_me_beautiful(0);
})();
