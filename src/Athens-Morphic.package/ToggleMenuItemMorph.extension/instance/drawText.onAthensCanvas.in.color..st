*Athens-Morphic
drawText: aStringOrText onAthensCanvas: aCanvas in: aRectangle color: stringColor
	| wrapper |
	"Draw the text on the canvas within the given bounds."
	wrapper := aCanvas asCanvasWrapper.
	(self enabled not and: [ self theme disabledItemStyle = #inset ])
		ifTrue: [ 
			wrapper
				drawString: aStringOrText
					in: aRectangle
					font: self fontToUse
					color: stringColor muchLighter lighter;
				drawString: aStringOrText
					in: (aRectangle translateBy: -1)
					font: self fontToUse
					color: stringColor ]
		ifFalse: [ 
			wrapper
				drawString: aStringOrText
				in: aRectangle
				font: self fontToUse
				color: stringColor ]