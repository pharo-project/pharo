rendering
athensFillRectangle: aRect on: anAthensCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	
	| torigin tcorner |

	"A gradient fill coordinates are first transformed by 
	paintTransform matrix,
	then
	by pathTransform matrix"
		
	torigin := anAthensCanvas paintTransform transform: origin.
	tcorner :=  anAthensCanvas paintTransform transform: corner.

	^ anAthensCanvas surface engine
		prepareForRendering;
		registerLinearGradient: colorRamp origin: torigin corner: tcorner;
		fillRectangle: aRect