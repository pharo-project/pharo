event handling
keyStroke: event
	"Pass on to the list."
	| indexToSelect |
	
	event keyCharacter = Character escape ifTrue: [ self hideList ].
	(self navigationKey: event) ifTrue: [^self].
	
	event keyCharacter == Character cr
		ifTrue: [ ^ self listSelectionIndex: self listSelectionIndex ].
		
	indexToSelect := self listMorph keyStroke: event.
	
	"If the returned value is not an integer, do not handle it"
	indexToSelect isInteger ifFalse: [ ^ self ].
	
	"If nothing found, do nothing"
	indexToSelect == 0 ifTrue: [ ^ self ].
	
	self listSelectionIndex: indexToSelect