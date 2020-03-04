class BobEvotar {
	constructor(my_dir) {
		this.video = null;
		this.timer = null;
		this.my_dir = my_dir;
	}
	// resolves when video ready to be played
	create_video() {
		const video = document.createElement("video");
		this.video = video;
		video.autoplay = false;
		video.controls = false;
		video.loop = true;
		video.preload = "metadata";
		video.src = this.my_dir + "/evotar/evotar.mp4";
		video.load();
		return video;
	}
	async prepare_video() {
		return new Promise(resolve => (
			this.video.addEventListener("canplay", resolve));
	}
	start_video() {
		this.video.play();
	}
	destroy_video() {
		this.video.pause();
		const parent = this.video.parentNode;
		if (parent)
			parent.removeChild(this.video);
		this.video = null;
	}
	async wait_for_time(video_time) {
		const diff = video_time - this.video.currentTime;
		if (diff <= 0)
			return;
		if (diff > 1)
			await new Promise(resolve => (
				setTimeout(resolve,
					   Math.floor(diff) * 1000 - 500)));

		return new Promise(resolve => {
			this.timer = event => {
				if (this.video.currentTime < video_time)
					return;
				resolve();
				this.video.removeEventListener("timeupdate",
							       this.timer);
			};
			this.video.addEventListener("timeupdate", this.timer);
		});
	}

	random_talk() {
		// yes, this evotar will talk. a lot.
	}
}

export { BobEvotar };
