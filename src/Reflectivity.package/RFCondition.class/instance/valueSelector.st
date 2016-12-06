accessing
valueSelector
	linkCondition numArgs isZero
		ifTrue: [ ^ #value ]
		ifFalse: [ ^ #cull: ]