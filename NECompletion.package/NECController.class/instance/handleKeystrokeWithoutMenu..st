keyboard
handleKeystrokeWithoutMenu: aKeyboardEvent
	
	self editor atCompletionPosition ifFalse: [ 
		^ (self smartInputWithEvent: aKeyboardEvent ) notNil ].	
	
	self stopCompletionDelay.

	(NECPreferences popupShowWithShortcut matches: {aKeyboardEvent})
		 ifTrue: [
			self openMenu.
			^true ].
		
	(self smartInputWithEvent: aKeyboardEvent )
		ifNotNil: [ ^true ].

	NECPreferences popupShowAutomatic ifTrue: [
		(aKeyboardEvent anyModifierKeyPressed not 
		and: [ aKeyboardEvent keyCharacter isCompletionCharacter ]) 
			ifTrue: [ self resetCompletionDelay ] ].
	
	^ false