bytecode generation
genStorePopInstVar: instVarIndex
	"200-207	11001 iii			Pop and Store Receiver Variable #iii
	 240		11110000	iiiiiiii	Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	(instVarIndex < 0 or: [instVarIndex > 7]) ifTrue:
		[^self genStorePopInstVarLong: instVarIndex].
	stream nextPut: 200 + instVarIndex