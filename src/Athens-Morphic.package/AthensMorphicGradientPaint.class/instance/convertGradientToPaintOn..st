private
convertGradientToPaintOn: aCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	
	^ gradient radial ifTrue: [
		aCanvas surface
			createRadialGradient: gradient colorRamp
			center:  (aCanvas pathTransform inverseTransform: gradient origin )
			radius: gradient direction y ]
	ifFalse: [
		aCanvas surface
			createLinearGradient: gradient colorRamp
			start: (aCanvas pathTransform inverseTransform: gradient origin)
			stop: (aCanvas pathTransform inverseTransform: gradient origin + gradient direction ) ].

