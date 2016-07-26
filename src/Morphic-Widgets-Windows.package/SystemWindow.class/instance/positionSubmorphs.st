open/close
positionSubmorphs
	"Feels like overkill, but effect needed"
	super positionSubmorphs.
	self submorphsDo:
		[:aMorph | aMorph positionSubmorphs]