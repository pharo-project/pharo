rendering
athensFillPath: aPath on: anAthensCanvas
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
		fillBezierShape: aPath contoursForFilling