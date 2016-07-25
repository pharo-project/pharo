event-handling
handlesKeyboard: anEvent
	^ anEvent keyCharacter = Character arrowDown
		or: [chooser notNil and: [ anEvent keyCharacter = Character escape]]