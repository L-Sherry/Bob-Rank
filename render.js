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
	// "too small ? use linear... not sure how much time this will happen."
	// turns out it happens a lot. But nearest is way too old-school ugly.
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

	render_opaque(textures_ranges) {
		select_buffer(this.context, this.buf);
		// three floats for the position, two floats for texture pos
		// total = 5
		const { pos, tex_coord } = this.locations_opaque;

		set_vertex_format(this.context, pos, 3, 5, 0);
		set_vertex_format(this.context, tex_coord, 2, 5, 3);

		if (textures_ranges === undefined)
			textures_ranges = this.textures_ranges;

		for (const textrange of textures_ranges) {
			console.assert(textrange.texture && textrange.size
				       && textrange.start !== undefined);
			assign_texture(this.context, textrange.texture);
			draw_triangles(this.context, textrange.start,
				       textrange.size);
		}
	}
	// assumes blending is already enabled
	render_blended(textures_ranges) {
		// maybe at one point we will put parameters in there ?
		// or maybe not.
		select_buffer(this.context, this.buf);

		const { pos, tex_coord, color_blend } = this.locations_blended;
		set_vertex_format(this.context, pos, 3, 5, 0);
		set_vertex_format(this.context, tex_coord, 2, 5, 3);

		if (textures_ranges === undefined)
			textures_ranges = this.blending_ranges;
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

export { webglplz, compile_shader, create_program, extractlocations, 
	 fill_const_buffer, fill_dynamic_buffer,
	 stuff_matrix_to_uniform,
	 disable_blending, enable_blending,
	 TextureTrove, BobRenderable };
