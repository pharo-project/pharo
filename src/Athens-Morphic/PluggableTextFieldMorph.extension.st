Extension { #name : #PluggableTextFieldMorph }

{ #category : #'*Athens-Morphic' }
PluggableTextFieldMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Eventually draw the helpText"
	super drawOnAthensCanvas: anAthensCanvas.
	self ghostText
		ifNotEmpty: [
			self text isEmpty
				ifTrue: [| ghostBounds |
					ghostBounds := ((self scroller submorphBounds topLeft corner: self innerBounds bottomRight)
						translateBy: 0 @ (self scroller height - self theme textFont height / 2) rounded)
						insetBy: self textMarginsWidth @ 0.
					anAthensCanvas 
						morphicDrawString: self ghostText
						in: ghostBounds
						font: self theme textFont
						color: Color lightGray]].
	entryCompletion 
		ifNotNil: [entryCompletion closeIfNotNeeded: self].
]
