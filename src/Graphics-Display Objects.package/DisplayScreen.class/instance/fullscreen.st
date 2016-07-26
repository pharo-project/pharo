other
fullscreen
	"Display fullscreen"

	ScreenSave notNil ifTrue: [Display := ScreenSave].
	clippingBox := super boundingBox