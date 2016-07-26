event handling
keyStroke: event 
	"Process keys navigation and space to toggle."
	
	(self navigationKey: event) ifTrue: [^self].
	event keyCharacter = Character space
		ifTrue: [self toggleSelected]