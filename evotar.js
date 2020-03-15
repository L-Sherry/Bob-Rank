class BobEvotar {
	constructor(my_dir) {
		this.video = null;
		this.audio1 = null;
		this.timer = null;
		this.cancel_timer = null;
		this.my_dir = my_dir;
		this.can_transition = null;
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
		video.src = this.my_dir + "/evotar/evotar.webm";
		video.load();

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

		const audio = document.createElement("audio");
		audio.autoplay = false;
		audio.controls = false;
		audio.loop = false;
		audio.preload = "metadata";
		audio.src = this.my_dir + "/evotar/loader_intro.opus";
		audio.load();

		this.audio1 = audio;

		return div;
	}
	async prepare_video() {
		this.video_overlay.style.opacity = "0";
		return new Promise(resolve => (
			this.video.addEventListener("canplay", resolve)));
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
		this.audio1.play();
	}
	freeze() {
		this.video.pause();
		this.video_overlay.style.opacity = "0.8";
	}
	destroy_video() {
		this.video.pause();
		this.audio1.pause();
		if (this.cancel_timer !== null)
			this.cancel_timer();

		const parent = this.container.parentNode;
		if (parent)
			parent.removeChild(this.container);
		this.container = null;
		this.video = null;
		this.video_overlay = null;
		this.audio1 = null;
		this.can_transition = null;
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
			console.log(`poisson(${avg_interval}): ${next} (${current_time} / ${duration})`);
		} while (this._enabled && current_time < duration);
		console.log("poisson done (enabled:", this._enabled);
	}

	async start_hi(duration, avg_interval) {
		const initial_count = 3 + Math.random() * 4;
		for (let i = 0; i < initial_count; ++i)
			this.add_hi_msg();

		this.start_poisson(duration, avg_interval, () => (
			this.add_hi_msg()
		));
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

		const msgid = Math.floor(Math.random() * 1.5 * messages.length);
		const msg = messages[msgid % messages.length];

		this.add_msg(nick, color, msg);
	}
	add_hi_msg() {
		this.add_random_msg([
			"Hi!", "hi", "hello", "heloo",
			"hello there", "it's time", "late as usual",
			"wait, already ?", "long time no see",
			"woohoo !", "DLC when ?", "switch when ?"
		]);
	}
	add_cool_msg() {
		this.add_random_msg([
			"woah", "wow", "cool", "awesome", "nice", "noice",
			"rekt", "oooh", "that's impressive", "WANT",
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
			"stream is broken for me",
			"wait",
			"Is that me or the stream is frozen ?",
			"lachsen are you ok ?",
			"wait lachsen is an evotar ?",
		]);
	}
}

export { BobEvotar, BobComments };
