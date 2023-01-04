Extension { #name : #MenuMorph }

{ #category : #'*Athens-Morphic' }
MenuMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Draw the menu. Add keyboard-focus feedback if appropriate"

	super drawOnAthensCanvas: anAthensCanvas.
	(self activeHand notNil and: [ self activeHand keyboardFocus == self and: [ self rootMenu hasProperty: #hasUsedKeyboard ] ])
		ifTrue: [
			(anAthensCanvas setStrokePaint: self theme menuKeyboardFocusColor) width: self theme menuBorderWidth.
			anAthensCanvas drawShape: self innerBounds ]
]
