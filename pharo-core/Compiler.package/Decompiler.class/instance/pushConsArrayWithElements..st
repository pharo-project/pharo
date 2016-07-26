instruction decoding
pushConsArrayWithElements: numElements 
	| array |
	self sawClosureBytecode.
	array := Array new: numElements.
	numElements to: 1 by: -1 do:
		[:i|
		array at: i put: stack removeLast].
	stack addLast: (constructor codeBrace: array)