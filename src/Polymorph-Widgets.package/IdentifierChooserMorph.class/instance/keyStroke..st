event handling
keyStroke: anEvent
	anEvent keyCharacter = Character escape 
		ifTrue: [self close. 
			requestor 
				ifNotNil: [requestor takeKeyboardFocus]. 
				^true].
	anEvent keyCharacter = Character arrowUp ifTrue: [self selectPreviousItem. ^true].
	anEvent keyCharacter = Character arrowDown ifTrue: [self selectNextItem. ^true].
	anEvent keyCharacter = Character arrowLeft ifTrue: [self switchToPreviousColumn. ^true].
	anEvent keyCharacter = Character arrowRight ifTrue: [self switchToNextColumn. ^true].
	(anEvent keyCharacter ~= Character cr 
		and: [anEvent keyCharacter < Character space]) ifTrue: [^false]. "ignore pageup/down etc."
	(anEvent keyCharacter = Character space or: [ anEvent keyCharacter = Character cr]) 
		ifTrue: [ ^self processEnter: anEvent ].
	^false