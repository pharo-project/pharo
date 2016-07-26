path commands
vLineTo: y
	^ self lineTo: (absolute ifTrue: [ endPoint x @ y] ifFalse: [ 0 @ y ])