keyboard
keyStroke: event 
	"Process spacebar for action and tab keys for navigation."
	
	(self navigationKey: event) ifTrue: [^self].
	(event keyCharacter = Character space or: [event keyCharacter = Character cr])
		ifTrue: [self performAction]