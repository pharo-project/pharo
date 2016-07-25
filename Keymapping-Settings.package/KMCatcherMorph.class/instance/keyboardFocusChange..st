event handling
keyboardFocusChange: aBoolean
	super keyboardFocusChange: aBoolean.
	focused := aBoolean.
	focused
		ifTrue: [ self color: (Smalltalk ui theme textEditorNormalFillStyleFor: self) ]
		ifFalse: [ self color: (Smalltalk ui theme textEditorDisabledFillStyleFor: self) ].
	^ true