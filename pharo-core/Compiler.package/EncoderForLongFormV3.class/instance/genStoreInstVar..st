bytecode generation
genStoreInstVar: instVarIndex
	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 64]) ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: instVarIndex.
		 ^self].
	self genStoreInstVarLong: instVarIndex