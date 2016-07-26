bytecode generation
genPushInstVar: instVarIndex
	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 64]) ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: instVarIndex.
		 ^self].
	self genPushInstVarLong: instVarIndex