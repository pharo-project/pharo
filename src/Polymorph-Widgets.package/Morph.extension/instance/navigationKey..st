*Polymorph-Widgets
navigationKey: event
	"Check for tab key activity and change focus as appropriate.
	Check for menu key to do popup."
	
	event isUserInterrupt ifTrue: [ ^ true ].

	(event keyCharacter = Character escape and: [
			event anyModifierKeyPressed]) ifTrue: [
		^ self yellowButtonActivity: false ].
	
	self window ifNotNil: [:win |
		(win handlesKeyStroke: event) ifTrue: [
			(win keyStroke: event) ifTrue: [^true]]].
	
	^false