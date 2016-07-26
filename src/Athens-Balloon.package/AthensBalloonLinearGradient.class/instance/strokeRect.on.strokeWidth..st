rendering
strokeRect: rect on: canvas strokeWidth: strokeWidth

	| torigin tcorner |

	"A gradient fill coordinates are first transformed by 
	paintTransform matrix,
	then
	by pathTransform matrix"
		
	torigin := canvas paintTransform transform: origin.
	tcorner :=  canvas paintTransform transform: corner.

	^ canvas surface engine
		prepareForRendering;
		registerLinearGradient: colorRamp origin: torigin corner: tcorner;
		strokeRectangle: rect width: strokeWidth