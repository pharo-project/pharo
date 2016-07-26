evaluating
cull: arg1 cull: arg2 cull: arg3
	^ selector numArgs < 3 
		ifTrue: [ self cull: arg1 cull: arg2 ]
		ifFalse: [ self value: arg1 value: arg2 value: arg3 ]