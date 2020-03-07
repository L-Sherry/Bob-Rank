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

		const audio = document.createElement("audio");
		audio.autoplay = false;
		audio.controls = false;
		audio.loop = false;
		audio.preload = "metadata";
		audio.src = this.my_dir + "/evotar/loader_intro.opus";
		audio.load();

		this.audio1 = audio;

		return video;
	}
	async prepare_video() {
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
		// maybe find a way to turn this white ?
		this.video.pause();
	}
	destroy_video() {
		this.video.pause();
		this.audio1.pause();
		if (this.cancel_timer !== null)
			this.cancel_timer();

		const parent = this.video.parentNode;
		if (parent)
			parent.removeChild(this.video);
		this.video = null;
		this.audio = null;
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
