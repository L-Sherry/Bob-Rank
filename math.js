// SPDX-Identifier: MIT

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

// Given 2d-projected x and y and assuming z = forced_z,
// find the x and y preimage in 3d space.
const get_preimage_partial = (proj_x, proj_y, forced_z, matrix) => {
	
	// this is tricky, you know that ?
	// first, we know z.
	// we know how much it contributes to w
	const w_base = forced_z * matrix[3][2] + matrix[3][3];
	// so w = x * m30 + y * m31 + w_base
	//
	// now, we want (x,y) such as:
	// (x * m00 + y * m01 + z*m02 + m03) / w = X
	// (x * m10 + y * m11 + z*m12 + m13) / w = Y
	//
	// well, that's easy, just multiply by w.
	// x * (m00 - X*m30) + y * (m01-X*m31) = w_base * X - z * m02 - m03
	// x * (m10 - Y*m30) + y * (m11-Y*m31) = w_base * Y - z * m12 - m13
	const A = [
		[ matrix[0][0] - proj_x * matrix[3][0],
		  matrix[0][1] - proj_x * matrix[3][1] ],
		[ matrix[1][0] - proj_y * matrix[3][0] /* 0 if no nudge */,
		  matrix[1][1] - proj_y * matrix[3][1] ]
	];
	const B = [ w_base * proj_x - forced_z * matrix[0][2] - matrix[0][3],
		    w_base * proj_y - forced_z * matrix[1][2] - matrix[1][3]];
	// cramer !
	const detA = A[0][0] * A[1][1] - A[0][1] * A[1][0];
	const x = (B[0] * A[1][1] - A[0][1] * B[1]) / detA;
	const y = (A[0][0] * B[1] - A[1][0] * B[0]) / detA;
	return { x, y };
};

export { mulmat, getcol, mulscalar, mulvec, addvec, mulvecnorm,
	 rotate_me, translate_matrix_before, translate_matrix, throw_at_wall,
	 get_preimage_partial };

