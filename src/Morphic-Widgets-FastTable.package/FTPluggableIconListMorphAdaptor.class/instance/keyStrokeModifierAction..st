event handling
keyStrokeModifierAction: event
	| args character |
	character := event keyCharacter.
	args := self keystrokeActionSelector numArgs.
	args = 1 ifTrue: [ 
		^ self model perform: self keystrokeActionSelector with: character].
	args = 2 ifTrue: [
		^ self model 
				perform: self keystrokeActionSelector
				with: character
				with: self ].
			
	^self error: 'keystrokeActionSelector must be a 1- or 2-keyword symbol'		
