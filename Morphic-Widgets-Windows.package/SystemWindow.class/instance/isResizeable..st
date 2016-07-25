controls
isResizeable: aBoolean

	aBoolean = isResizeable ifTrue: [ ^ self ].
	aBoolean
		ifTrue: [ self beResizeable ]
		ifFalse: [ self beUnresizeable ]