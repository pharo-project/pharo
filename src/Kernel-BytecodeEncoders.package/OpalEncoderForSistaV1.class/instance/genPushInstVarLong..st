bytecode generation
genPushInstVarLong: instVarIndex
	"226		11100010	i i i i i i i i	Push Receiver Variable #iiiiiiii (+ Extend A * 256)"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex < 0 or: [instVarIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 65535].
	instVarIndex > 255 ifTrue:
		[self genUnsignedSingleExtendA: instVarIndex // 256].
	stream
		nextPut: 226;
		nextPut: instVarIndex \\ 256