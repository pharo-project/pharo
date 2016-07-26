instruction decoding
pushNewArrayOfSize: size
	self sawClosureBytecode.
	stack addLast: #pushNewArray -> (Array new: size)