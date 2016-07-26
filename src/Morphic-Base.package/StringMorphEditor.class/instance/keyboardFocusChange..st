event handling
keyboardFocusChange: aBoolean
	| hadFocus |
	owner ifNil: [ ^self ].
	hadFocus := owner hasFocus.
	super keyboardFocusChange: aBoolean.
	aBoolean ifFalse:
		[hadFocus ifTrue:
			[owner lostFocusWithoutAccepting; doneWithEdits].
		^ self delete]