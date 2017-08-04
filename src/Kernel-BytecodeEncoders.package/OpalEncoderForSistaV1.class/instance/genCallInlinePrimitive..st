extended bytecode generation
genCallInlinePrimitive: primitiveIndex
	"248		11111000	i i i i i i i i	1jjjjjjj		Call Primitive #iiiiiiii + (jjjjjjj * 256)"
	"N.B. We could have made CallPrimitive a 2-byte code taking an extension, but that would
	 complicate the VM's determination of the primitive number and the primitive error code
	 store since the extension, being optional, would make the sequence variable length."
	(primitiveIndex < 1 or: [primitiveIndex > 32767]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 32767].
	stream
		nextPut: 248;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8) + 128