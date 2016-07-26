initialization
initPixelRampCache
	"Create an LRUCache to use for accessing pixel ramps.
	When a new pixel ramp is needed, a temporary GradientFillStyle
	is created so that it can be used to create a new pixel ramp"

	^ PixelRampCache := LRUCache new 
		maximumWeight: 32;
		factory: [ :key | 
			(GradientFillStyle new colorRamp: key) 
				computePixelRampOfSize: 512 ];
		yourself