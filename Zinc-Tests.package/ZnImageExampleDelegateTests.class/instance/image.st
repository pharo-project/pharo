private
image
	^ ZnClient new
		beOneShot;
		get: 'http://zn.stfx.eu/zn/Hot-Air-Balloon.gif';
		entity