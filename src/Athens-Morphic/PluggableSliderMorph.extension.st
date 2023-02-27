Extension { #name : #PluggableSliderMorph }

{ #category : #'*Athens-Morphic' }
PluggableSliderMorph >> drawOnAthensCanvas: aCanvas [

	super drawOnAthensCanvas: aCanvas.
	aCanvas
		morphicDrawString: self label
		in: self labelBounds
		font: self font
		color: self fontColor
]
