arithmetic
quo: aNumber 
	"Primitive. Divide the receiver by the argument and answer with the 
	result. Round the result down towards zero to make it a whole integer. 
	Fail if the argument is 0 or is not a SmallInteger. Optional. See Object 
	documentation whatIsAPrimitive."
	<primitive: 13>
	aNumber = 0 ifTrue: [^ (ZeroDivide dividend: self) signal].
	(aNumber isMemberOf: SmallInteger)
		ifFalse: [^ super quo: aNumber].
	(aNumber = -1 and: [self = self class minVal])
		ifTrue: ["result is aLargeInteger" ^ self negated].
	self primitiveFailed