as yet unclassified
genCallPrimitive: primitiveIndex
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)