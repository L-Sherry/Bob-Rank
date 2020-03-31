// SPDX-Identifier: MIT

import { getcol, mulscalar, mulvec, addvec } from "./math.js";

import { fill_const_buffer, fill_dynamic_buffer,
	 TextureTrove, BobRenderable, BobRender } from "./render.js";

// maybe there is a way to iteratize it ?
const forEachBackward = (array, callback, from) => {
	if (from === undefined) {
		from = array.length - 1;
		console.assert(from !== undefined, "wtf");
	}
	for (let idx = from; idx >= 0; --idx)
		callback(array[idx], idx, array);
};

function* reversed(array, end = null) {
	if (end === null)
		end = array.length;
	while (end --> 0)
		yield array[end];
}

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

// Geometry helpers to make tiles
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
		case "SLOPE_WEST_WALL_NORTH":
			// FIXME: handle slope/wall mixins.
			ret.topleft[2] += size_y;
			ret.bottomleft[2] += size_y;
			break;
		case "SLOPE_EAST":
		case "SLOPE_EAST_WALL_NORTH":
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
		case "WALL_NW_END":
			bottomleft = [x, y, z];
			bottomright = [x + size_x, y - size_y, z - size_y];
			break;
		case "WALL_NE":
		case "WALL_NE_END":
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
		case "WALL_NORTH_END":
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
		case "SLOPE_WEST_WALL_NORTH":
		case "SLOPE_EAST_WALL_NORTH":
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
		case "WALL_NE_END":
		case "WALL_NW_END":
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

		// ok, here goes.
		// we calculate texture coordinates. These are interpolated
		// for each fragment. but the highest fragment will receive
		// a pixel exactly representing the first pixel of the next
		// tile... bad.  Most solutions revolves around "well, just
		// put an extra pixel in yur texture when making yur atlas",
		// but this is crap.
		//
		// however, we have the particularity to use nearest pixel
		// filtering (to show off those pixels), so substracting half
		// a pixel should be enough to prevent this.
		//
		// TODO: due to some early fuckupery, total_width and
		// total_height are crap.
		const OFFSET_FOR_HIGH = 1/1024;
		const left = tile_x_pos / total_width;
		const right
			= left + tile_size_x / total_width - OFFSET_FOR_HIGH;
		const top_ = tile_y_pos / total_height;
		const bottom
			= top_ + tile_size_y / total_height - OFFSET_FOR_HIGH;
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
		const high_x = base_x + Math.round(block.coll.size.x / 16);
		const base_y = Math.floor(block.coll.pos.y / 16);
		const high_y = base_y + Math.round(block.coll.size.y / 16);

		const base_z = Math.floor(block.coll.pos.z / 16);
		const top_z = base_z + Math.round(block.coll.size.z / 16);

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

	// shift a tile's z coordinate while keeping the same 2d screen
	// coordinate.
	static shift_to_z(tile, wanted_map_z) {
		tile.map_y += wanted_map_z - tile.map_z;
		tile.map_z = wanted_map_z;
	}

	// the actual dreadful height reconstruction algorithm.
	// tiles contains a list of tiles at the current position on the screen
	// south contains a list of tiles at the screen position just below us
	// left contains a list of tiles at the screen position just left of us
	//
	// this adjusts the tiles array in-place, as well as changing the map_z
	// of tiles in it.
	handle_tiles_build_map(tiles, left, south) {
		if (tiles.length === 0)
			return;

		const shift_to_z
			= this.constructor.shift_to_z.bind(this.constructor);
		if (south)
			south = Array.from(south);
		else
			south = [];

		// cleanse the south
		south = south.filter((south_tile, index) => {

			// hidden block or decorative tile
			if (south_tile.tile === undefined
			    || !south_tile.true_tile)
				return false;
			// what can be seen, can never be unseen.
			if (index
			    && south[index - 1].map_z === south_tile.map_z)
				return false;


			const { quad_type } = south_tile;
			const z_action
				= MoreTileInfos.z_action_on_tile(quad_type);
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
				const ret = { z_action: "keepz_north",
					      from_z: current_block.map_z,
					      certitude: 20 };
				current_block = null;
				return ret;
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
					/*if (map_z === my_tile.map_z)
						certain = 10;
					else*/
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
			if (my_tile.quad_type.startsWith("IGNORE_"))
				return ret;

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
			my_tile.orig_map_z = my_tile.map_z;

			// this sometimes happen with object layers... and
			// sometimes other things too.  This should prevent
			// double-raising.
			if (last_tile !== null
			    && last_tile.orig_map_z === my_tile.orig_map_z
			    && last_tile.quad_type === my_tile.quad_type) {
				// reuse last, don't even attempt to guess.
				my_tile.map_z = last_tile.map_z;
				my_tile.map_y = last_tile.map_y;
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
				my_heightinfo.type = my_tile.quad_type;
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

			const next_level = levels[levelno + 1];

			const z_min = level.height;
			const z_max = next_level ? next_level.height : 9999999;

			const map_infos = [];
			for (const map of level.maps) {
				const distance = Number(map.distance);
				if (distance !== 1)
					continue;
				if (!map.tiles.data)
					continue;
				const mapinfo = this.get_mapinfo(map, z_min,
								 z_max);
				mapinfo.map = map;
				map_infos.push(mapinfo);
			}

			if (!map_infos.length)
				continue;
			ret.push(map_infos);
		}
		return ret;
	}

	// may return null
	static find_main_tile_in_maps(map_infos, screen_x, screen_y) {
		const ret = {
			map_x: screen_x, map_y: null, map_z: null,
			quad_type: null, true_tile: false, map_info: null
		};

		// "in theory, there shouldn't be more than one true tile
		// per level", i once said. Screw that, that's false. some
		// bottom wall are actually transparent, so there is both a
		// ground and the begining of a wall.
		//
		// How to resolve it ? well, iterate backward and pick the wall.
		for (const map_info of reversed(map_infos)) {
			const tile = map_info.map.data[screen_y][screen_x] - 1;
			if (tile === -1)
				continue;
			const quad_type = map_info.tileinfo.get_type(tile);
			if (quad_type === "DELETE")
				continue;
			if (quad_type.startsWith("IGNORE_")) {
				if (ret.quad_type)
					continue;
				// this tile is useless for height
				// reconstruction, but it still needs to be
				// drawn, so it needs a position.
				ret.tile = tile;
				ret.quad_type = quad_type;
				ret.map_z = map_info.min_map_z;
				ret.map_y = screen_y + ret.map_z;
				ret.map_info = map_info;
			} else {
				ret.tile = tile;
				ret.quad_type = quad_type;
				ret.true_tile = true;
				ret.map_z = map_info.min_map_z;
				ret.map_y = screen_y + ret.map_z;
				ret.map_info = map_info;
				return ret;
			}
		}
		if (!ret.quad_type)
			return null;
		return ret;
	}

	// screen_x and y are in "tiles" unit and are positions on the screen
	// returns an array sorted by ascending "z-index".
	// entries are:
	// {map_x, map_y, map_z,
	//  tile (number), true_tile: true or false depending on tile status,
	//  map_info
	//  tileinfo of map, map_info
	// }
	find_tiles_at_pos(base_maps, screen_x, screen_y,
			  hiddenblocks, layerviews) {

		// a hidden block is interesting if we are on top of
		// it, nothing else (for now)
		const blocks = [];
		for (const block_info of hiddenblocks) {
			const clipped
				= BobMap.clip_hidden_block(block_info,
							   screen_x,
							   screen_y);
			if (clipped)
				blocks.push(clipped);
		}

		const tiles = [];
		const find_main_tile = this.constructor.find_main_tile_in_maps;
		for (const map_infos of base_maps) {

			const tile_pos = find_main_tile(map_infos, screen_x,
							screen_y);
			if (tile_pos)
				tiles.push(tile_pos);
		}

		for (const objectlayer of layerviews) {
			const { min_scr_y, max_scr_y, map_infos } = objectlayer;
			if (!(min_scr_y <= screen_y && screen_y < max_scr_y))
				continue;

			const tile_pos = find_main_tile(map_infos, screen_x,
							screen_y);
			if (tile_pos)
				tiles.push(tile_pos);
		}

		const all = blocks.concat(tiles);
		// sort by ascending z (even if we iterate it backward
		// later ...)
		// and put hidden blocks after the rest, because the
		// reconstruction algo needs to iterate on them first.
		sortWithKey(all, i => i.map_z + (i.block ? 0.5 : 0));
		return all;
	}


	make_draw_map(base_maps) {
		if (!base_maps.length)
			return null;
		//const absolute_map_z_min = base_maps[0].min_map_z;

		const find_tiles = this.find_tiles_at_pos.bind(this, base_maps);
		const shift_to_z
			= this.constructor.shift_to_z.bind(this.constructor);

		const current_wall = [];
		let floating_wall;

		const update_wall = (tiles) => {
			if (tiles.length === 0) {
				// the wall ended up with nothing.
				current_wall.length = 0;
				floating_wall = true;
				return;
			}
			// maybe check that tiles.length is 1 ?
			const last_tile = tiles[tiles.length - 1];
			if (!last_tile.quad_type) // there is no tile here.
				return;
			if (floating_wall
			    && last_tile.quad_type.startsWith("WALL_")) {
				current_wall.push(last_tile);
				return;
			}
			// catches BORDER_SOUTH, BORDER_SW and even
			// BORDER_SE_FLAT.
			if (!last_tile.quad_type.startsWith("BORDER_S")) {
				// sorry, not a floating wall.
				current_wall.length = 0;
				floating_wall = false;
			}
			if (!floating_wall) {
				if (last_tile.quad_type.startsWith("BORDER_N"))
					floating_wall = true;
				return;
			}
			// we are at the end of a floating wall.
			// now take the border, assume its z is correct and
			// adjust the height of the walls they are connected to.
			// This should only adjust floating walls (not
			// connected to a ground)
			let map_z = last_tile.map_z;
			while (current_wall.length)
				shift_to_z(current_wall.pop(), --map_z);
		};

		const draw_map = [];
		for (let scr_x = 0; scr_x < this.mapWidth; ++scr_x) {
			const hidden_block_col = this.hiddenblocks.filter(b => (
				b.base_x <= scr_x && scr_x < b.high_x
			));
			const layer_views_col = this.layerviews.filter(lv => (
				lv.min_scr_x <= scr_x && scr_x < lv.max_scr_x
			));
			const draw_col = new Array(this.mapHeight);
			draw_map.push(draw_col);

			// first element is, many times, a floating wall.
			current_wall.length = 0;
			floating_wall = true;

			for (let scr_y = this.mapHeight; scr_y --> 0;) {
				const left = scr_x ? draw_map[scr_x-1][scr_y]
						   : null;
				const south = draw_col[scr_y + 1];
				const tiles = find_tiles(scr_x, scr_y,
							 hidden_block_col,
							 layer_views_col);
				draw_col[scr_y] = tiles;

				update_wall(tiles);

				this.handle_tiles_build_map(tiles, left, south);
			}
		}
		return draw_map;
	}

	draw_tile(result_vector, tileno, draw_info, map_info) {
		if (tileno === -1)
			return;
		const { map_x, map_y, map_z, quad_type } = draw_info;
		const { tilesize } = map_info;
		const x = map_x * tilesize;
		const y = map_y * tilesize + tilesize;

		// The Z Fighting War has begun.
		// Only the strongest will survive.
		// Which mean, not you. Sorry, map.
		// but entities are stronger than you and must be on top of you.
		const z = map_z * tilesize - 0.5;

		const quad_vertex
			= BobGeo.make_quad_vertex(x, y, z, quad_type,
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
			min_map_z, object_map: null
		};
	}

	draw_one_map(result_vector, map, draw_map, level) {
		const tile_has_level = tile_pos => (
			tile_pos.map_info.object_map === null
			&& tile_pos.map_info.z_min === level
		);

		// note: we need the map_info of the current map, not the
		// map_info of the map used by the draw map algorithm (which
		// may not be the same)
		const map_info = this.get_mapinfo(map, level, level + 42);
		const get_tile = tiles => tiles.find(tile_has_level);

		const draw_tile = this.draw_tile.bind(this, result_vector);

		let count = 0;
		draw_map.forEach((draw_col, scr_x) => {
			draw_col.forEach((draw_info, scr_y) => {
				const tile = map.data[scr_y][scr_x] - 1;
				if (tile === -1)
					return;
				const tile_pos = get_tile(draw_info);
				if (!tile_pos)
					// can happen for DELETE tiles.
					return;
				draw_tile(tile, tile_pos, map_info);
				++count;
			});
		});

		return count * 6; // we add two triangles of 3 vertex each time
	}

	draw_walls(result_vector, map_info) {

		const { min_map_z } = map_info;
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
					map_x,
					map_y,
					map_z,
					quad_type
				};
				if (tileno === null)
					return;
				this.draw_tile(result_vector, tileno, draw_info,
					       map_info);
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
				if (to_z < min_map_z)
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

	steal_map_entities() {
		for (const entity of ig.game.entities) {
			if (entity instanceof ig.ENTITY.HiddenBlock) {
				const block_info
					= BobMap.hidden_block_info(entity);
				this.hiddenblocks.push(block_info);
			} else if (entity instanceof ig.ENTITY.ObjectLayerView)
				this.layerviews.push({ entity });
			else if (entity instanceof ig.ENTITY.OLPlatform) {
				// Object Layer Platform could be one of our
				// arch-nemesis, since it uses tiles from the
				// object map that moves !
				// but, based on its current usage, it is
				// probably impossible to have it move while
				// on S-Rank.
				// places where it might be visible in S-Rank:
				// - jungle lost shrine
				// - bergen trail excluded tops and bull cave
				// - heat cliff temple
				// - naval quest in rookie hurburr
				this.layerviews.push({ entity });
			}
			// thanks god, i don't have to handle TeleportStairs or
			// BossPlatform, since they only happen in places where
			// you can't get to S-Rank
		}

		const find_level = (searched_map) => {
			for (const levelname in ig.game.levels) {
				const level = ig.game.levels[levelname];
				for (const map of level.maps)
					if (map === searched_map)
						return level.maps;
			}
			console.assert(false, "could not find object map");
			return [searched_map];
		};

		sortWithKey(this.layerviews,
			    layerview => layerview.entity.maps[0].tiles.path);
		this.layerviews.map((layerview, i) => {

			// fun fact: ig.game.getObjectMaps is hardcoded
			// to return an array with a single map, which
			// is what ends up here.
			const map = layerview.entity.maps[0];
			const { tilesize } = map;
			const { pos, size } = layerview.entity.coll;
			layerview.min_scr_x = Math.floor(pos.x / tilesize);
			const size_x = Math.ceil(size.x / tilesize);
			layerview.max_scr_x = layerview.min_scr_x + size_x;

			const map_y = Math.floor(pos.y / tilesize);
			const size_y = Math.ceil(size.y / tilesize);

			const map_z = Math.floor(pos.z / tilesize);
			const size_z = Math.ceil(size.z / tilesize);

			layerview.min_scr_y = map_y - map_z - size_z;
			layerview.max_scr_y = map_y + size_y - map_z;

			// now, i need the actual level of the object layer view
			// because the game does not give it.

			const maps = find_level(map);
			// i have to construct a map_info for each oddball z
			// position given by the object layer view... for each
			// map.
			const min_z = pos.z;
			const max_z = pos.z + size.z;

			layerview.map_infos = maps.map(map => {
				const ret = this.get_mapinfo(map, min_z, max_z);
				ret.object_map = i;
				ret.map = map;
				return ret;
			});
			// used to match map info to us.
			layerview.index = i;
		});
	}

	make_layerviews_draw(result_vector, current_i, draw_map) {
		const handle_layerview_tile = (scr_x, scr_y, index,
					       map_info) => {
			const tile = map_info.map.data[scr_y][scr_x] - 1;
			if (tile === -1)
				return;

			const tile_pos
				= draw_map[scr_x][scr_y].filter(tile_pos => (
					tile_pos.map_info.object_map === index
				));
			if (tile_pos.length !== 1) {
				console.assert(false, "can't find back tile");
				return;
			}
			this.draw_tile(result_vector, tile, tile_pos[0],
				       map_info);
			current_i += 6;
		};

		const iterate_xy = (min_scr_x, max_scr_x, min_scr_y, max_scr_y,
				    callback) => {
			for (let scr_x = min_scr_x; scr_x < max_scr_x; ++scr_x)
				for (let scr_y = min_scr_y; scr_y < max_scr_y;
				     ++scr_y)
					callback(scr_x, scr_y);
		};

		for (const layerview of this.layerviews) {
			let { min_scr_x, min_scr_y, max_scr_x, max_scr_y,
			      index, map_infos } = layerview;
			const any_map = map_infos[0].map;

			// this is nonsense, but it happens.
			min_scr_x = Math.max(min_scr_x, 0);
			min_scr_y = Math.max(min_scr_y, 0);
			max_scr_x = Math.min(max_scr_x, any_map.width);
			max_scr_y = Math.min(max_scr_y, any_map.height);
			const iterate_tiles
				= iterate_xy.bind(null, min_scr_x, max_scr_x,
						  min_scr_y, max_scr_y);

			layerview.tex_ranges = [];
			let tex_range = {};

			for (const map_info of map_infos) {
				if (!map_info.map.tiles.data)
					continue;
				const path = map_info.map.tiles.path;
				if (path !== tex_range.path) {
					const texture
						= this.texture_trove.get(path);
					tex_range = { start: current_i, texture,
						      mode: "normal", alpha: 1,
						      blend_color: [0,0,0,0] };
				}
				iterate_tiles((scr_x, scr_y) =>
					handle_layerview_tile(scr_x, scr_y,
							      index, map_info));
				const size = current_i - tex_range.start;
				if (size && !tex_range.size)
					layerview.tex_ranges.push(tex_range);
				tex_range.size = size;
			}
			// we may fail to pick up OLPlatform tiles here and
			// there, don't fail hard on them.
			if (!(layerview.entity instanceof ig.ENTITY.OLPlatform))
				console.assert(layerview.tex_ranges.length,
					       "layerview without tiles ?");
		}
		return current_i;
	}

	steal_map() {
		// steal the entities usefuls for a map.
		this.steal_map_entities();

		// AAHHHH where are my quads ? they take away my display lists,
		// and now they take away my quads too ?
		// i have to do TRIANGLES ? TRIANGLES SUCKS ! QUADROGUARD FTW !
		const everything = [];
		let i = 0;

		// should probably migrate to vertex indexes. at least they
		// kept this. Except with my inconsistent st coordinates, it
		// would probably be kind of useless.

		const textures_ranges = this.textures_ranges;
		textures_ranges.length = 0;

		const base_maps = this.base_maps_by_z(ig.game.levels,
						      ig.game.maxLevel);
		const draw_map = this.make_draw_map(base_maps);

		if (!draw_map) {
			// happens at start
			this.texture_trove.cleanup();
			fill_const_buffer(this.context, this.buf, everything);
			return;
		}

		// find all required textures

		// really need a map with insertion order
		const texture_obj = { order: []};
		const add_texture_of_level = (level) => {
			for (const map of level.maps) {
				if (!(map instanceof ig.MAP.Background))
					continue;
				if (map.tiles.path in texture_obj)
					continue;
				const path = map.tiles.path;
				const img = map.tiles.data;
				if (!img)
					continue;
				const texture = this.texture_trove.add(path,
								       img);
				texture_obj[path] = texture;
				texture_obj.order.push(path);
			}
		};
		forEachBackward(ig.game.levels, add_texture_of_level,
				ig.game.maxLevel - 1);
		// i'm iterating on the same things, but also the object maps.
		for (const levelname in ig.game.levels)
			add_texture_of_level(ig.game.levels[levelname]);

		const get_info_if_base_map = maybe_base => {
			for (const map_infos of base_maps)
				for (const map_info of map_infos)
					if (map_info.map === maybe_base)
						return map_info;
			return null;
		};

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

		i = this.make_layerviews_draw(everything, i, draw_map);

		this.texture_trove.cleanup();

		fill_const_buffer(this.context, this.buf, everything);

		// now i have the map ! time to find the treasure !
	}

	render_objectlayerviews(blending_mode) {
		const ranges_to_draw = [];
		for (const layerview of this.layerviews) {
			if (layerview.entity._hidden)
				continue;
			const any_sprite = layerview.entity.sprites[0];
			if (!any_sprite) {
				console.warn("layer view with no sprite");
				continue;
			}
			const alpha = Number(any_sprite.alpha);
			const blended = alpha !== 1;
			if (blending_mode !== blended)
				continue;
			for (const tex_range of layerview.tex_ranges) {
				tex_range.alpha = alpha;

				ranges_to_draw.push(tex_range);
			}
		}
		if (blending_mode)
			this.render_blended(ranges_to_draw);
		else
			this.render_opaque(ranges_to_draw);
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
		this.hyperheight_by_z = [];
		this.hyperheight_ranges = [];
		this.textures_ranges.length = 0;
	}
	do_overrides(path, cubesprite, overriden) {
		if (cubesprite.bobrank_set_z !== undefined) {
			const z = cubesprite.bobrank_set_z;
			const y = cubesprite.pos.y - cubesprite.pos.z + z;
			const x = cubesprite.pos.x;
			overriden.pos = { x, y, z };
		}

		// I have LOADS of reserves on how the game classify ground
		// and wall sprites.
		// i will list them ALL HERE ! MUAHAHAHA !
		const splitted = path.split("/");
		const override = walk_break_on_first(path_overrides, splitted);
		if (override) {
			overriden.type = override; // for now.
			return;
		}

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
				overriden.type = "wall";
		} else if (!cubesprite.size.z)
			overriden.type = "ground";
	}
	prepare_sprites(spritearray, hyperz_value, set_z_value) {
		if (ig.system.context.globalAlpha !== 1)
			console.log("global alpha not one !");
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
			const texture = this.texture_trove.add(path, image);

			let has_opaque = true;
			let has_blending = false;
			if (cs.gui && cs.pos.z >= hyperz_value) {
				cs.bobrank_set_z = set_z_value;
				this.hyperheight_by_z.push({sprite, path});
				continue;
			} else
				delete cs.bobrank_set_z;
			if (cs.renderMode
			    || Number(cs.alpha) !== 1
			    || cs.overlay.color) {
				has_opaque = false;
				has_blending = true;
			}
			if (cs.lighterOverlay.color)
				has_blending = true;

			if (has_blending)
				this.blend_sprites_by_z.push({ sprite, path });

			if (!has_opaque)
				continue;


			let textureinfo = by_texture[path];
			if (!textureinfo) {
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
				size = Math.min(patch_size - src_i,
						total_size - i);
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
		const is_game_ground = sprite.ground;
		let is_ground = is_game_ground;
		// i have some reserves on how the game classify ground
		// sprites from wall sprites.
		// FIXME: this affects ground removal, is that intended ?
		let overriden = {};
		this.do_overrides(path, cs, overriden);
		switch (overriden.type) {
			case "ground":
				is_ground = true;
				break;
			case "wall":
				is_ground = false;
				break;
		}
		const pos = overriden.pos || cs.pos;

		const src_quad_tex
			= BobEntities.get_src_quad_tex(cs, is_game_ground);

		if (!src_quad_tex)
			return 0;

		// BobGeo want low x, high y, low z
		let x = pos.x + cs.tmpOffset.x + cs.gfxOffset.x +
			cs.gfxCut.left;
		let y = (pos.y + cs.tmpOffset.y + cs.size.y
			       + cs.gfxOffset.y);
		let z = pos.z + cs.tmpOffset.z;
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
	finalize_blending_sprites(result_vector, i, blend_sprites, ranges) {
		const get_color = color_calculator.get.bind(color_calculator);
		ranges.length = 0;
		for (const blend_sprite of blend_sprites) {
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
				ranges.push({
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
				// FIXME: this is appantly broken, shoot frozen
				// water bubble to test.
				// It looks like we shouldn't draw the base
				// sprite, but i don't understand why.
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
		i = this.finalize_blending_sprites(everything, i,
						   this.blend_sprites_by_z,
						   this.blending_ranges);
		this.finalize_blending_sprites(everything, i,
					       this.hyperheight_by_z,
					       this.hyperheight_ranges);

		fill_dynamic_buffer(this.context, this.buf, everything);
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
			"WALL_NE", "WALL_NORTH", "WALL_NW",
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
			if (is_default)
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
			return "IGNORE_GROUND";
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
			ret = { get_type: () => "IGNORE_GROUND",
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
		case "DELETE":
			return "delete"; // delete them, do not even draw them.
		case "BORDER_SOUTH":
		case "BORDER_SW":
		case "BORDER_SE":
		case "BORDER_SW_FLAT":
		case "BORDER_SE_FLAT":
		case "BORDER_EAST":
		case "BORDER_WEST":
		case "SLOPE_WEST":
		case "SLOPE_EAST":
		case "SLOPE_EAST_WALL_NORTH":
		case "SLOPE_WEST_WALL_NORTH":
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
		// those are "top of walls", not followed by grounds.
		case "WALL_NORTH_END":
		case "WALL_NE_END":
		case "WALL_NW_END":
			return "fall_north"; // fall and go north
		default:
			throw "unknown tileinfo" + tileinfo;
		}
	}
}

class BobGame {
	constructor(on_srank_cb, on_no_srank_cb, on_resize_cb, on_enemy_killed,
		    on_player_killed) {
		this.renderer = new BobRender();
		this.moretileinfo = null;
		this.map = null;
		this.entities = null;
		this._enabled = null;
		this.canvas3d = null;
		this.canvas_gui = null;
		this.context_gui = null;
		this.on_srank_cb = on_srank_cb;
		this.on_no_srank_cb = on_no_srank_cb;
		this.on_resize_cb = on_resize_cb;
		this.on_enemy_killed = on_enemy_killed;
		this.on_player_killed = on_player_killed;
	}

	enable() {
		if (this._enabled === true)
			return;
		this._enabled = true;
		this.clear_screen_and_everything();
	}
	disable() {
		if (this._enabled === false)
			return;
		this._enabled = false;
	}

	async setup(canvas3d, canvas2dgui) {
		this.renderer.setup_canvas(canvas3d);

		// TODO: put this in BobRenderable
		const opaque_locations = {
			pos: this.renderer.locations.pos,
			tex_coord: this.renderer.locations.texcoord
		};
		const blend_locations = {
			pos: this.renderer.blend_locations.pos,
			tex_coord: this.renderer.blend_locations.texcoord,
			color_blend: this.renderer.blend_locations.blend_color
		};

		this.moretileinfo = await new Promise((resolve, reject) => {
			$.ajax({
				dataType:"json",
				url:"data/more-tile-infos.json",
				success: d => resolve(new MoreTileInfos(d)),
				error: reject
			});
		});

		this.map = new BobMap(this.renderer.context,
				      opaque_locations,
				      blend_locations,
				      this.moretileinfo);
		this.entities = new BobEntities(this.renderer.context,
						opaque_locations,
						blend_locations,
						this.moretileinfo);

		this.canvas3d = canvas3d;
		this.canvas_gui = canvas2dgui;

		// depends on pixel scaling
		this.canvas_gui.width = ig.system.realWidth;
		this.canvas_gui.height = ig.system.realHeight;

		this.resize(ig.system.screenWidth, ig.system.screenHeight);

		this.context_gui = canvas2dgui.getContext("2d");
		this.context_gui.imageSmoothingEnabled = false;
		this.context_gui.scale(ig.system.contextScale,
				       ig.system.contextScale);


		this.disable();

		// Call my modelChanged please.
		// sc.model is defined in game.feature.model.game-model
		// but it is an addon, initialized only when the game starts.
		sc.Model.addObserver(sc.model, this);

	}

	draw_layerz (parent) {
		if (!this._enabled) {
			parent();
			return;
		}
		if (!this.map) {
			console.assert(ig.game.maps.length === 0);
			return;
		}
		if (ig.game.mapRenderingBlocked || ig.loading
		    || !(ig.game.maxLevel > 0))
			return;

		this.renderer.start_non_blending();
		this.renderer.enable_depth();

		this.entities.clear();
		const camera_z = this.renderer.camera_center.z;
		const high_z = camera_z + 128;
		this.entities.prepare_sprites(ig.game.renderer.spriteSlots,
					      high_z, camera_z);
		this.entities.prepare_sprites(ig.game.renderer.guiSpriteSlots,
					      high_z, camera_z);
		this.entities.finalize();
		this.map.render_opaque();
		this.map.render_objectlayerviews(false);
		this.entities.render_opaque();

		// START ALPHA BLENDONG
		this.renderer.start_blending();

		this.map.render_objectlayerviews(true);
		this.entities.render_blended();

		this.renderer.disable_depth();
		this.entities.render_blended(this.entities.hyperheight_ranges);
	}

	get_screen_from_map_pos(parent, result, map_screen_x, map_screen_y) {
		if (!this._enabled)
			return parent(result, map_screen_x, map_screen_y);
		const ret = this.renderer.get_screen_from_map_pos(map_screen_x,
								  map_screen_y);
		result.x = ret.x;
		result.y = ret.y;
		return result;
	}
	get_map_from_screen_pos(parent, result, screen_x, screen_y) {
		if (!this._enabled)
			return parent(result, screen_x, screen_y);

		const ret = this.renderer.get_map_from_screen_pos(screen_x,
								  screen_y);

		result.x = ret.x;
		// Of course, if this was this simple....
		// The game compare those coordinates with ... { x, y - z }
		// so i need to do the same thing.
		result.y = ret.y - this.renderer.camera_center.z;

		// note: one of the caller is PlayerCrossHairController
		// and passes coll.pos as parameter... but it does not
		// use it for its sprite :(
		return result;
	}

	prepare_draw(parent) {
		if (!this._enabled)
			return parent();
		// this huge hack here is an example of things that you
		// shouldn't do.
		const orig_zoom = ig.system.zoom;
		// with that zoom value, prepareDraw() should think all sprites
		// are visible. Say bye bye to your FPS, but it turns out
		// a 3d view can see a lot more than a 2d view.
		ig.system.zoom = 0.000001;
		const ret = parent();
		ig.system.zoom = orig_zoom;
		return ret; // actually unused, but who knows.
	}

	bind_to_game() {
		const me = this;
		const modulize = (dummyname, deps, func) =>
			ig.module(dummyname).requires(...deps).defines(func);
		modulize("bobrender", ["impact.base.renderer"], () => {
			ig.Renderer2d.inject({
				drawLayers: function (force, dont_clear) {
					const parent
						= this.parent.bind(this, force,
								   dont_clear);
					return me.draw_layerz(parent);
				},
				prepareDraw: function(entities, force) {
					const parent
						= this.parent.bind(this,
								   entities,
								   force);
					return me.prepare_draw(parent);
				},
				drawPostLayerSprites: function(force) {
					const parent
						= this.parent.bind(this, force);
					if (!me._enabled)
						return parent();
					return null;
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
				},
				onPostDraw: function() {
					me.draw_gui();
				}
			});
			ig.addGameAddon(() => new BobRankAddon());
		});
		modulize("bobscreen", ["impact.base.system"], () => {
			ig.System.inject({
				getScreenFromMapPos: function(res, x, y) {
					const p = this.parent.bind(this);
					return me.get_screen_from_map_pos(p,
									  res,
									  x, y);
				},
				getMapFromScreenPos: function(res, x, y) {
					const p = this.parent.bind(this);
					return me.get_map_from_screen_pos(p,
									  res,
									  x, y);
				},
				setCanvasSize: function(width, height,
							noborder) {
					this.parent(width, height, noborder);
					me.on_resize_cb(width, height);
					//me.resize(width, height);
				}
			});
		});
		modulize("bobcomment", ["game.feature.combat.combat"], () => {
			sc.Combat.inject({
				onCombatantDeathHit: function(murderer,
							      murdered) {
					this.parent(murderer, murdered);
					me.on_entity_murdered(murdered);
				}
			});
		});
	}

	modelChanged(observed, whatchanged /*, value*/) {
		if (observed !== sc.model)
			return;
		if (!(whatchanged === sc.GAME_MODEL_MSG.COMBAT_RANK_CHANGED
		      || whatchanged === sc.GAME_MODEL_MSG.COMBAT_MODE_CHANGED))
			return;
		if (observed.isSRank())
			// S-Raaaaaaaaank !
			this.on_srank_cb();
		else
			// oh noes, no more S-Rank.
			this.on_no_srank_cb();
	}

	on_entity_murdered(entity) {
		if (entity.party === sc.COMBATANT_PARTY.ENEMY)
			this.on_enemy_killed();
		else if (entity.isPlayer)
			this.on_player_killed();
	}

	draw_gui() {
		if (!this._enabled)
			return;
		const stolen_context = ig.system.context;
		const stolen_canvas = ig.system.canvas;
		ig.system.context = this.context_gui;
		ig.system.canvas = this.canvas_gui;
		this.context_gui.clearRect(0, 0, this.canvas_gui.width,
					   this.canvas_gui.height);
		ig.gui.renderer.draw();
		ig.system.context = stolen_context;
		ig.system.canvas = stolen_canvas;
	}

	clear_screen_and_everything() {
		if (!this._enabled)
			return;
		//const centerx = ig.camera._currentZoomPos.x;
		//let centery = ig.camera._currentZoomPos.y;
		let camera_x = ig.camera._currentPos.x;
		let camera_y = ig.camera._currentPos.y;
		let camera_z = 0;
		const targets = ig.camera.targets;
		if (targets.length) {
			const last = targets[targets.length - 1];
			if (last.target && last.target._currentZ) {
				camera_z = last.target._currentZ;
				camera_y += camera_z;
			}
		}
		const zoom = ig.camera._currentZoom || 1;

		this.renderer.set_camera_center(camera_x, camera_y, camera_z,
						zoom);
		this.renderer.clear_screen();
	}

	resize(width, height) {
		if (!this.canvas3d)
			return;
		this.canvas3d.width = width;
		this.canvas3d.height = height;
		this.renderer.set_size(width, height);
	}
}

import { BobEvotar, BobComments } from "./joke.js";

class BobRank {
	constructor(my_dir) {
		this.bobgame = new BobGame(this.start.bind(this),
					   this.stop.bind(this, false),
					   this.resize.bind(this),
					   this.enemy_killed.bind(this),
					   this.stop.bind(this, true));
		this.bobgame.bind_to_game();

		this.my_dir = my_dir;
		this.evotar = new BobEvotar(my_dir);
		this.step = "";
		this.stop_events = null;
	}

	create_html() {

		this.original_canvas = ig.system.canvas;

		const fullsize = element => {
			Object.assign(element.style, {
				position: "absolute",
				width: "100%",
				height: "100%"
			});
		};

		// this contains everything
		const div = document.createElement("div");
		const backgroundurl
			= encodeURI(this.my_dir+"joke/background.png");
		Object.assign(div.style, {
			position: "absolute",
			left: "0", right:"0", top:"0", bottom:"0",
			margin:"auto",
			display: "none",
			backgroundImage: `url("${backgroundurl}")`,
			backgroundSize: "cover"
		});
		this.whole_div = div;

		// this contains the 3d game
		const game = document.createElement("div");
		this.game_div = game;
		Object.assign(game.style, {
			position: "absolute", top: "0", left: "0",
			width: "83%", height: "83%"
		});
		div.appendChild(game);

		const canvasplz = () => {
			let canvas = document.createElement("canvas");
			fullsize(canvas);
			game.appendChild(canvas);
			canvas.style.display = "none";
			return canvas;
		};
		this.canvas3d = canvasplz();
		this.canvas2dgui = canvasplz();
		this.canvas2dgui.style.zIndex = 1;

		// right pane, 18% size
		const pane = document.createElement("div");
		this.pane_div = pane;
		Object.assign(pane.style, {
			position: "absolute", right: "0", top: "0",
			height: "100%", width: "18%"
		});

		const list = document.createElement("img");
		Object.assign(list.style, {
			marginLeft: "auto",
			marginRight: "auto",
			marginTop: "25px",
			display:"block",
			width:"50%"
		});
		list.src = this.my_dir + "joke/list.png";
		pane.appendChild(list);

		this.comments = new BobComments();
		const chatbox = this.comments.create();
		pane.appendChild(chatbox);

		div.appendChild(pane);

		document.getElementById("game").appendChild(div);
	}

	static async sleep(millisecs) {
		return new Promise(resolve => setTimeout(resolve, millisecs));
	}

	async initial_setup() {
		await this.bobgame.setup(this.canvas3d, this.canvas2dgui);
	}

	async start() {
		try {
			await this.do_start();
		} catch (error) {
			console.error("Failure in Bob-Rank", error);
			this.stop(false, true);
			throw error;
		}
	}

	async do_start() {
		if (this.step !== "")
			return;

		this.step = "wait";
		// start the fluff
		const video = this.evotar.create();
		const video_promise = this.evotar.prepare_video();
		Object.assign(video.style, {
			maxWidth: "100%", maxHeight: "100%",
			position: "relative"
		});
		this.create_events();
		await this.constructor.sleep(1500);
		if (this.step !== "wait")
			return;
		ig.slowMotion.add(0, 2, "ZOMGBOBRANK");
		ig.bgm.pause(ig.BGM_SWITCH_MODE.VERY_SLOW);

		this.step = "fadeout";
		await this.constructor.sleep(5000);
		await video_promise;
		if (this.step !== "fadeout")
			return;
		// this is unreliable
		ig.soundManager.pushPaused();
		// FIXME: player can still interact with everything


		this.game_div.appendChild(video);
		this.evotar.run_the_evotar();
		this.comments.reset_enable();
		this.comments.start_hi(35000, 4000);

		this.original_canvas.opacity = "0%";
		this.whole_div.style.display = "block";

		// the original canvas behind all this is still at its true
		// size, but we need to reduce its screen width and height
		// so that it calculates mouse coordinates correctly.
		ig.system.screenWidth = ig.system.screenWidth * 0.83;
		ig.system.screenHeight = ig.system.screenHeight * 0.83;


		this.step = "start";
		await this.evotar.wait_for_transition();
		if (this.step !== "start")
			return;
		this.step = "fake_load";
		this.pane_div.insertBefore(video, this.pane_div.firstChild);
		this.canvas2dgui.style.display = "";

		await this.do_fake_load_animation();
		if (this.step !== "fake_load")
			return;
		this.step = "running";
		this.canvas3d.style.display = "";
		this.comments.start_cool(5000, 1000);
		ig.soundManager.popPaused();

		ig.slowMotion.clearNamed("ZOMGBOBRANK", 3);
		ig.bgm.resume(ig.BGM_SWITCH_MODE.MEDIUM);

		this.bobgame.enable();
	}

	create_events() {
		if (this.stop_events !== null)
			return;

		const steps = [
			// play a evotar destroyed sound
			{ type: "PLAY_SOUND", speed: 0.3, global: true,
			  sound:"media/sound/misc/countdown-1.ogg"},
			{ type: "PLAY_SOUND", global: true,
			  sound:"media/sound/scenes/bomb-explosion.ogg" }
		];
		// those sounds need some loading before they can play
		this.stop_events
			= steps.map(step => new ig.EVENT_STEP[step.type](step));
	}

	async stop(player_dies, no_ar) {
		if (this.step === "" || this.step === "destroying")
			return;

		this.step = "destroying";
		this.stop_events[0].start();
		this.evotar.freeze();
		this.comments.add_ohno_msg();
		this.comments.start_ohno(4000, 500);

		await this.constructor.sleep(player_dies ? 3200 : 2000);
		this.stop_events[1].start();
		await this.constructor.sleep(500);
		this.step = "";

		// must undo everything, at whatever step we are.
		this.canvas3d.style.display = "none";
		this.canvas2dgui.style.display = "none";
		this.original_canvas.opacity = "100%";
		this.whole_div.style.display = "none";
		this.evotar.destroy_video();
		ig.soundManager.popPaused();
		ig.slowMotion.clearNamed("ZOMGBOBRANK");
		ig.bgm.resume(ig.BGM_SWITCH_MODE.FAST);
		// recalculate the original screenWidth
		sc.options._setDisplaySize();

		const crashedmsg
			= "CrossCode 2 [Evotar Lachsen]: Unexpected Error";

		const ar_msg = new ig.EVENT_STEP.SHOW_AR_MSG(
			{ text: crashedmsg, entity: {player:true}, time: 5,
			  mode: "NO_LINE", color: "RED" }
		);
		if (!no_ar)
			ar_msg.start();

		this.bobgame.disable();
		this.comments.disable();
		this.step="";
	}
	async enemy_killed() {
		if (this.step === "")
			return;

		await this.constructor.sleep(2500 + Math.random() * 1500);

		this.comments.add_cool_msg();
	}

	async do_fake_load_animation() {
		const context = this.canvas2dgui.getContext("2d");
		const BAR_WIDTH = 370;
		const BAR_HEIGHT = 20;
		// center that
		const BAR_X = Math.floor((ig.system.width - BAR_WIDTH) / 2);
		const BAR_Y = Math.floor((ig.system.height - BAR_HEIGHT) / 2);

		context.fillStyle = "black";
		context.fillRect(0, 0, ig.system.width, ig.system.height);
		// sie sehen...
		context.fillStyle = "#ddd";
		context.fillRect(BAR_X - 3, BAR_Y - 3,
				 BAR_WIDTH + 6, BAR_HEIGHT + 6);
		// ...den neuen...
		context.fillStyle = "black";
		context.fillRect(BAR_X - 2, BAR_Y - 2,
				 BAR_WIDTH + 4, BAR_HEIGHT + 4);
		context.fillStyle = "#ddd";

		const TOTAL_TIME = 4500;
		const STEPS = 8;
		const STEP_NOMINAL_TIME = TOTAL_TIME / STEPS;
		let current_time_like = 0;
		const BAR_NOMINAL_STEP = BAR_WIDTH / STEPS;
		for (let i = 0; i < STEPS; ++i) {

			let next_time = i * STEP_NOMINAL_TIME;
			next_time
				+= (Math.random()-0.5) * STEP_NOMINAL_TIME / 3;
			const wait = next_time - current_time_like;

			let bar = i * BAR_NOMINAL_STEP;
			bar += (Math.random() - 0.5) * BAR_NOMINAL_STEP / 2;
			bar = Math.max(Math.min(bar, BAR_WIDTH), 0);
			context.fillRect(BAR_X, BAR_Y, bar, BAR_HEIGHT);

			await this.constructor.sleep(wait);
			current_time_like = next_time;
		}
	}


	resize(width, height) {
		if (!this.whole_div)
			return;
		this.whole_div.style.width = width + "px";
		this.whole_div.style.height = height + "px";

		width = width * 0.83;
		height = height * 0.83;
		this.bobgame.resize(width, height);
	}
}

export default class Mod extends Plugin {
	constructor(my_description) {
		super(my_description);
		this.description = my_description;
	}
	preload() {
	}
	postload() {
		let basedir = this.description.baseDirectory;
		const assetz = basedir.indexOf("assets/");
		if (assetz !== -1)
			basedir = basedir.slice(assetz + "assets/".length);

		this.bobrank = new BobRank(basedir);
	}
	async main() {
		this.bobrank.create_html();
		await this.bobrank.initial_setup();
	}
}
