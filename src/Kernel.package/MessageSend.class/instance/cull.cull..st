evaluating
cull: arg1 cull: arg2
	^ selector numArgs < 2 
		ifTrue: [ self cull: arg1]
		ifFalse: [ self value: arg1 value: arg2 ]