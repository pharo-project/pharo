rendering
athensFillRectangle: aRect on: anAthensCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	
	| c dir n |

	c := anAthensCanvas paintTransform transform: center.
	dir :=  (radius@0).
	n := (0@radius).
	
	^ anAthensCanvas surface engine
		prepareForRendering;
		registerRadialGradient: colorRamp center: c direction: dir normal: n;
		fillRectangle: aRect