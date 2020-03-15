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

export { BobEvotar };
