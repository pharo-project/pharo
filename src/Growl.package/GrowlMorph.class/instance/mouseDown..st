interaction
mouseDown: evt
	super mouseDown: evt.
	evt yellowButtonPressed ifTrue: [^ self].
	actionBlock ifNotNil: [actionBlock valueWithPossibleArgs: { self }].