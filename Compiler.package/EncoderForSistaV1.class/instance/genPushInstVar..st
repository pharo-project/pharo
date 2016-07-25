bytecode generation
genPushInstVar: instVarIndex
	(instVarIndex between: 0 and: 15) ifTrue:
		["0-15 	0000iiii 	Push Receiver Variable #iiii"
		 stream nextPut: 0 + instVarIndex.
		 ^self].
	self genPushInstVarLong: instVarIndex