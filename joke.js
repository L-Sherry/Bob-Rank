// SPDX-Identifier: MIT
const randint = x => Math.floor(x * Math.random());

class BobEvotar {
	constructor(my_dir) {
		this.video = null;
		this.current_audio = null;
		this.next_audio = null;
		this.playlist = [];
		this.playlist_index = null;
		this.timer = null;
		this.cancel_timer = null;
		this.my_dir = my_dir;
		this.can_transition = null;

		this.playlist_data = [];
		$.ajax({
			dataType:"json",
			url: this.make_url_join("joke/playlist.json"),
			success: d => {
				this.playlist_data = d;
			},
			error: e => console.warn("cannot load playlist", e)
		});
	}
	random_playlist() {
		const sounds = [];
		for (const playlist_name in this.playlist_data) {
			const playlist = this.playlist_data[playlist_name];
			for (const sound of playlist.sounds || [])
				sounds.push({playlist_name, sound});
		}
		for (let i = 1; i < sounds.length; ++i) {
			const pick_from = i + randint(sounds.length - i);
			sounds.splice(i - 1, 0, sounds.splice(pick_from, 1)[0]);
		}
		return sounds;
	}
	new_playlist() {
		this.playlist.length = 0;
		this.playlist_index = -1;
		const add_random_intro = playlist_name => {
			const { intros } = this.playlist_data[playlist_name];
			if (!intros || !intros.length)
				return;
			const sound = intros[randint(intros.length)];
			this.playlist.push({playlist_name, sound});
		};
		// INTRO must not have any sounds, only 64k intros.
		add_random_intro("INTRO");
		const sounds = this.random_playlist();
		const added_playlist = {};
		for (const sound_info of sounds) {
			const { playlist_name } = sound_info;
			if (added_playlist[playlist_name] === undefined) {
				add_random_intro(playlist_name);
				added_playlist[playlist_name] = true;
			}
			this.playlist.push(sound_info);
		}
	}
	make_url_join(...paths) {
		const last = paths.pop();
		paths.unshift(this.my_dir);
		return paths.map(component => (
			component.endsWith("/") ? component : component + "/"
		)).join("") + last;
	}
	next_sound() {
		if (this.playlist.length === 0)
			return;
		// swap them
		const next_audio = this.current_audio;
		this.current_audio = this.next_audio;
		this.next_audio = next_audio;

		++this.playlist_index;
		if (this.playlist_index >= this.playlist.length) {
			this.playlist.length = 0;
			// don't need the intros, they have been played already.
			this.playlist.push(... this.random_playlist());
			this.playlist_index = 0;
		}

		const next = this.playlist[this.playlist_index];
		const subdir = this.playlist_data[next.playlist_name].dir;
		next_audio.src = this.make_url_join("joke", subdir,
						    next.sound.url);
		// it should really be audio/opus, but that fails, for like,
		// half of the sounds.
		this.force_load_media(next_audio, "audio/ogg");

		if (this.current_audio.src)
			// what to do if still not loaded ?
			this.current_audio.play().catch(() => {});
	}
	// resolves when video ready to be played
	create() {
		console.assert(this.video === null);
		const video = document.createElement("video");
		this.video = video;
		video.autoplay = false;
		video.controls = false;
		video.loop = true;
		video.preload = "metadata";
		video.src = this.make_url_join("joke/evotar.webm");

		Object.assign(video.style, {
			maxWidth: "100%", maxHeight: "100%", width: "100%"
		});

		const overlay = document.createElement("div");
		this.video_overlay = overlay;
		const text = document.createElement("p");
		Object.assign(overlay.style, {
			backgroundColor:"#86f", opacity: "0%", zIndex: "1",
			position: "absolute", top: "0", left: "0",
			right: "0", bottom:"0"
		});
		text.textContent = "Unexpected Error";
		// text.textContent = "Unerwarteter Felher"
		Object.assign(text.style, {
			fontSize: "2vh", fontFamily: "serif",
			backgroundColor:"black", border:"1px red solid",
			textAlign: "center",
			width: "8em", height: "3ex", position: "absolute",
			left: "calc(50% - 4em)", top: "calc(50% - 1.5ex)"
		});


		const div = document.createElement("div");
		this.container = div;
		div.appendChild(video);
		div.appendChild(overlay);
		overlay.appendChild(text);

		const audioplz = () => {
			const audio = document.createElement("audio");
			audio.autoplay = false;
			audio.controls = false;
			audio.loop = false;
			audio.preload = "metadata";
			audio.addEventListener("ended",
					       () => this.next_sound());
			return audio;
		};

		this.current_audio = audioplz();
		this.next_audio = audioplz();
		this.new_playlist();
		// this loads the first sound, but does not play it.
		this.next_sound();

		return div;
	}
	async force_load_media(media_element, mime) {
		const try_load = () => {
			media_element.load();
			return new Promise((resolve, reject) => {
				media_element.oncanplaythrough = resolve;
				media_element.onerror = reject;
			}).finally(() => {
				media_element.oncanplaythrough = null;
				media_element.onerror = null;
			});
		};
		try {
			await try_load();
		} catch (fail) {
			console.log("second chance for",media_element.src);
			// ok, second chance.
			// This horror has been sponsored by CORS corporation.
			const response = await fetch(media_element.src);
			const blob = await response.blob();

			const reader = new FileReader();
			await new Promise((resolve, reject) => {
				reader.onload = resolve;
				reader.onerror = reject;
				reader.readAsDataURL(blob);
			});
			const url = reader.result;
			const data_index = url.indexOf(";base64");

			media_element.src
				= `data:${mime}${url.slice(data_index)}`;
			await try_load();
		}
	}

	async prepare_video() {
		this.video_overlay.style.opacity = "0";
		await this.force_load_media(this.video, "video/webm");
	}
	async run_the_evotar() {
		let transition_is_ok = null;
		this.can_transition
			= new Promise(resolve => transition_is_ok = resolve);
		this.video.play();
		await this.wait_for_time(35.9);
		if (this.video === null)
			return;
		transition_is_ok();
		this.video.muted = true;
		// this plays the first sound (and loads the second).
		this.next_sound();
	}
	freeze() {
		this.video.pause();
		this.current_audio.pause();
		this.video_overlay.style.opacity = "0.8";
	}
	destroy_video() {
		this.video.pause();
		this.current_audio.pause();
		if (this.cancel_timer !== null)
			this.cancel_timer();

		const parent = this.container.parentNode;
		if (parent)
			parent.removeChild(this.container);
		this.container = null;
		this.video = null;
		this.video_overlay = null;
		this.current_audio = null;
		this.next_audio = null;
		this.can_transition = null;
		this.playlist.length = 0;
	}
	remove_timer() {
		this.video.removeEventListener("timeupdate", this.timer);
		this.timer = null;
	}
	async wait_for_time(video_time) {
		const diff = video_time - this.video.currentTime;
		if (diff <= 0)
			return null;
		if (diff > 1)
			await new Promise(resolve => (
				setTimeout(resolve,
					   Math.floor(diff) * 1000 - 500)));

		return new Promise((resolve, reject) => {
			this.cancel_timer = () => {
				reject();
				this.remove_timer();
			};
			this.timer = () => {
				if (this.timer === null)
					return;
				if (this.video.currentTime < video_time)
					return;
				resolve();
				this.remove_timer();
			};
			this.video.addEventListener("timeupdate", this.timer);
		});
	}
	async wait_for_transition() {
		console.assert(this.can_transition);
		await this.can_transition;
	}
}

// The best part of the internet.
class BobComments {
	constructor() {
		this._enabled = false;
		this.can_hi = false;
	}
	create() {
		const container = document.createElement("div");
		// the most awesome of responsive designs.
		Object.assign(container.style, {
			position: "absolute", top:"66.5%", bottom:"0",
			left:"7.4%", right:"0", overflow: "hidden",
			// hack to add round border to contained
			border: "0.8vh solid black", borderRadius: "1.5vh"
		});
		const inner = document.createElement("div");
		Object.assign(inner.style, {
			position: "absolute", bottom: "0",
			left: "0", right: "0",
		});

		container.appendChild(inner);

		this.msgdiv = inner;

		return container;
	}
	keep_n_msgs(n) {
		while (this.msgdiv.childElementCount > n)
			this.msgdiv.removeChild(this.msgdiv.children[0]);
	}
	reset_enable() {
		this._enabled = true;
		this.keep_n_msgs(0);
		this.add_msg("Chat", "white",
			     "joined channel: radicalfishgames");
	}
	disable() {
		this._enabled = false;
		this.keep_n_msgs(0);
	}

	async start_poisson(duration, avg_interval, cb) {
		// POISSON D'AVRIL !
		let current_time = 0;
		do {
			if (current_time && cb() === false)
				return;
			// max is around 4
			const wait = -Math.log(Math.max(Math.random(), 0.018));
			const next = wait * avg_interval;
			await new Promise(resolve => setTimeout(resolve, next));
			current_time += next;
		} while (this._enabled && current_time < duration);
	}

	async start_hi(duration, avg_interval) {
		const initial_count = 3 + Math.random() * 4;
		for (let i = 0; i < initial_count; ++i)
			this.add_hi_msg();

		await this.start_poisson(duration, avg_interval, () => (
			this.add_hi_msg()
		));
		// more a hack than a solution, but one of the sounds need this
		this.add_msg("Kovacp911", "#b60",
			     "@RadicalFishGames The future project will have"+
			     " a dynamic aspect ratio in mind ?");
	}
	async start_cool(duration, avg_interval) {
		this.start_poisson(duration, avg_interval, () => (
			this.add_cool_msg()
		));
	}
	async start_ohno(duration, avg_interval) {
		this.start_poisson(duration, avg_interval, () => (
			this.add_ohno_msg()
		));
	}

	add_msg(nick, nick_color, msg) {
		const p = document.createElement("p");
		Object.assign(p.style, {
			fontFamily: "sans-serif", fontSize: "1.3vh",
			/*fontWeight: "bold",*/
			background: "#333", color: "#ddd",
			borderTop: "1px black solid", padding: "0.3vh"
		});
		// the best in XSS protection
		p.innerHTML = `<span style="color: ${nick_color}"
			       >${nick}</span>: ${msg}`;
		this.msgdiv.appendChild(p);
		this.keep_n_msgs(18);
	}
	add_random_msg(messages) {
		const names = ig.database.data.names;
		const id = Math.floor(Math.random() * names.length);
		const nick = ig.LangLabel.getText(names[id].name);
		const colors = ["#35f","#0b3", "#b0a", "#b60", "#0a0"];
		// best hash
		const color = colors[nick.charCodeAt(1) % colors.length];

		// of course, this tends to repeat messages. may need to keep
		// an history
		const msgid = Math.floor(Math.random() * 1.5 * messages.length);
		const msg = messages[msgid % messages.length];

		this.add_msg(nick, color, msg);
	}
	add_hi_msg() {
		this.add_random_msg([
			"Hi!", "hi", "hello", "heloo", "hey guys", "hello hon",
			"hello in there", "hi guyz", "hey, hello", "yes sampai",
			"sup", "yay it starts", "Greetings", "let's see",
			"yeah !", "woohoo !", "i can't wait for henne's jokes",
			"What will it be this time ?", "it is time", "dun",
			"Let me guess, working on a new map ?", "hello RFGchan",
			"hello there", "it's time", "late as usual", "Nanni ?",
			"wait, already ?", "long time no see",
			"oh wait, i'm late",
			"it's been a long time since i was last here, and " +
				"this is much better than last time.",
			"Is the DLC release date finally announced ?",
			"DLC when ?", "any news about the switch release ?",
			"let's hope the wifi works this time",
		]);
	}
	add_cool_msg() {
		this.add_random_msg([
			"woah", "wow", "cool", "awesome", "nice", "noice",
			"rekt", "oooh", "that's impressive", "WANT", "NOICE",
			"Looks cool.", "Looks great.", "0v0", "OwO", "UwU",
			"That's awesome guys.",
			"OK, you got us all hyped, ETA ?",
			"this will be awesome",
			"so this is the new project ?",
			"What game is this ?", "What is this stream about ?",
			"What graphic card will it require ?",
			// leaSmug
		]);
	}
	add_ohno_msg() {
		this.add_random_msg([
			"What's happenning ?",
			"what",
			"oh no",
			"omg",
			"lag",
			"omg wtf",
			"what's going on",
			"uuuuh",
			"That's a bug, right ?",
			"stream is broken for me",
			"wait",
			"Is that me or the stream is frozen ?",
			"lachsen are you ok ?",
			"wait lachsen is an evotar ?",
		]);
	}
}

export { BobEvotar, BobComments };
