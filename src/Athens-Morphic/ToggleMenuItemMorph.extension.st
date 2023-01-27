Extension { #name : #ToggleMenuItemMorph }

{ #category : #'*Athens-Morphic' }
ToggleMenuItemMorph >> drawKeyTextOnAthenCanvas: aCanvas [
	"Draw the key text on the canvas."

	self keyText ifNil: [^self].
	(ToggleMenuItemShortcut
		owner: self
		keyText: self keyText)
		drawOnAthensCanvas: aCanvas
]

{ #category : #'*Athens-Morphic' }
ToggleMenuItemMorph >> drawOnAthensCanvas: anAthensCanvas [

	super drawOnAthensCanvas: anAthensCanvas.
	self drawKeyTextOnAthenCanvas: anAthensCanvas
]

{ #category : #'*Athens-Morphic' }
ToggleMenuItemMorph >> drawText: aStringOrText onAthensCanvas: aCanvas in: aRectangle color: stringColor [
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
]
