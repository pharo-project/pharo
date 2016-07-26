accessing
pixelRamp
	"Compute a pixel ramp, and cache it for future accesses"

	^ pixelRamp ifNil: [
		"Ask my cache for an existing instance or to create one"
		pixelRamp := self class pixelRampCache at: colorRamp ]