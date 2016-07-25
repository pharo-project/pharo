*Athens-Core
asAthensPaintOn: anAthensCanvas

	^ AthensMorphicGradientPaint new gradient: self.
	"^ radial ifTrue: [
		anAthensCanvas surface
			createRadialGradient: colorRamp
			center:  (anAthensCanvas pathTransform inverseTransform: origin )
			radius: direction y ]
	ifFalse: [
		anAthensCanvas surface
			createLinearGradient: colorRamp
			origin: (anAthensCanvas pathTransform inverseTransform: origin)
			corner: (anAthensCanvas pathTransform inverseTransform: origin + direction ) ]
"