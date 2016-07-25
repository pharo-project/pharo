private
addBytecode: description 
	"Append to the receiver a description of the bytecode, instruction." 

	| bytes symbolic |
	
	bytes := ByteArray new: (scanner pc - oldPC).
	
	(oldPC to: scanner pc - 1) doWithIndex: [:each :index | 
		bytes at: index put: (method at: each)].
	
	 symbolic := SymbolicBytecode new 
		method: method;
		bytes: bytes;
	 	description: description;
		offset: oldPC.
	symbolicBytecodes add: symbolic.
	oldPC := scanner pc.
