drawing
drawOn: aCanvas 
	"Eventually draw the helpText"
	super drawOn: aCanvas.
	self ghostText
		ifNotEmpty: [
			self text isEmpty
				ifTrue: [| ghostBounds |
					ghostBounds := ((self scroller submorphBounds topLeft corner: self innerBounds bottomRight)
						translateBy: 0 @ (self scroller height - self theme textFont height / 2) rounded)
						insetBy: self textMarginsWidth @ 0.
					aCanvas 
						drawString: self ghostText
						in: ghostBounds
						font: self theme  textFont
						color: Color lightGray]].
	entryCompletion 
		ifNotNil: [entryCompletion closeIfNotNeeded: self].