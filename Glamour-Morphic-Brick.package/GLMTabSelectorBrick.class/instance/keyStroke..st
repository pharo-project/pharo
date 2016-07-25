event handling
keyStroke: event 
	"Process keys navigation and space to toggle."

	(self navigationKey: event) ifTrue: [ ^ self ].
	event keyCharacter = Character arrowLeft
		ifTrue: [ self selectPreviousTab ].
	event keyCharacter = Character arrowRight
		ifTrue: [ self selectNextTab ]