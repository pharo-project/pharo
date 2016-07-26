event-handling
keystroke: anEvent from: aMorph
	(self handlesKeyboard: anEvent) 
		ifFalse: [^ false].
	(chooser notNil and: [ anEvent keyCharacter = Character escape])
		ifTrue: [self closeChooser. 
			aMorph takeKeyboardFocus.
			^ true].
	(chooser isNil and: [ anEvent keyCharacter = Character arrowDown])
		ifTrue: [previousToken := nil.
			self openChooserWith: aMorph textMorph text string from: aMorph.
			chooser ifNotNil: [^ true]].
	^ chooser 
		ifNil: [false]
		ifNotNil: [chooser keyStroke: anEvent]
	