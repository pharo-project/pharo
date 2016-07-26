rendering
athensFillPath: aPath on: anAthensCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	
	| c |

	c := anAthensCanvas paintTransform transform: center.

	^ anAthensCanvas surface engine
		prepareForRendering;
		registerRadialGradient: colorRamp center: c radius: radius;
		fillBezierShape: aPath contoursForFilling
