evaluating
cull: arg
	^ selector numArgs = 0 
		ifTrue: [ self value ]
		ifFalse: [ self value: arg ].