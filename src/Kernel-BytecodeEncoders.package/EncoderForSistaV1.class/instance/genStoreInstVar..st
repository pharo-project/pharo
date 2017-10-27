bytecode generation
genStoreInstVar: instVarIndex
	"243		11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	self genStoreInstVarLong: instVarIndex