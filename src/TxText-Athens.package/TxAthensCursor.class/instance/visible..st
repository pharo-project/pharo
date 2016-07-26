accessing
visible: aBoolean

	super visible: aBoolean.
	aBoolean ifTrue: [  self startStepping ].
