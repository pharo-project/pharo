bytecode generation
genStorePopInstVar: instVarIndex
	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 64]) ifTrue:
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: instVarIndex.
		 ^self].
	self genStorePopInstVarLong: instVarIndex