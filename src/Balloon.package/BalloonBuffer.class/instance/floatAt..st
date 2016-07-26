accessing
floatAt: index
	"For simulation only"
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)