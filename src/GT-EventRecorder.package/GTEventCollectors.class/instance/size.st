accessing
size
	^ mutex critical: [ collectors size ]