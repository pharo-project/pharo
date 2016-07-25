event handling
updateVisibility
	(self textArea isNil or: [ self textArea canChangeText not ])
		ifTrue: [ ^ self beNotVisible ].
	self beVisible.
	self isBlinkingCursor
		ifTrue: [ self startBlinking ]
		ifFalse: [ self beVisible ].
	self changed