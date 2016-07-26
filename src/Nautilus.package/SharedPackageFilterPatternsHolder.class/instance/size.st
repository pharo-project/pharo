accessing
size

	^ mutex critical: [ value size ]