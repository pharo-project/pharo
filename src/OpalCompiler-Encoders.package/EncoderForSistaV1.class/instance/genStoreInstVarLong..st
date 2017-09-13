bytecode generation
genStoreInstVarLong: instVarIndex
	"243		11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	(instVarIndex < 0 or: [instVarIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 65535].
	instVarIndex > 255 ifTrue:
		[self genUnsignedSingleExtendA: instVarIndex // 256].
	stream
		nextPut: 243;
		nextPut: instVarIndex \\ 256