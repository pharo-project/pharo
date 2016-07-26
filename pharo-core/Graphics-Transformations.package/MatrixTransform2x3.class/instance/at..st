accessing
at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)