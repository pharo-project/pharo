initialization
pixelRampCache
	"Allow access to my cache of pixel ramps."

	^ PixelRampCache ifNil: [ self initPixelRampCache ]