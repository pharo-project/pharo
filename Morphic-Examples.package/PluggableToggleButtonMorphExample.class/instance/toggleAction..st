actions
toggleAction: aBoolean
	self inform: (aBoolean
		ifTrue: [ 'Button pressed' ]
		ifFalse: [ 'Button unpressed' ])