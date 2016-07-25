path commands
hLineTo: x
	^ self lineTo: (absolute ifTrue: [ x @ endPoint y] ifFalse: [ x @ 0 ])