updating
update: anAspect

	anAspect == #textDPIChanged ifTrue: [
		AbstractFont allSubInstancesDo: [ :font | font pixelsPerInchChanged ]].