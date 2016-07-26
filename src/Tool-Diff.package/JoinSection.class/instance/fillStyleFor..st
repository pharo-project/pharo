as yet unclassified
fillStyleFor: rect
	"Answer the fillStyle to use for the given rectangle."
	
	^self src color = self dst color
		ifTrue: [self src color]
		ifFalse: [(GradientFillStyle ramp: {0.0 -> self src color. 1.0 -> self dst color})
				direction: rect width@0;
				origin: rect topLeft]