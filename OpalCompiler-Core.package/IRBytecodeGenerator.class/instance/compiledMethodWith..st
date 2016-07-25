results
compiledMethodWith: trailer
	| lits header quickPrimitive |
	lits := self literals.
	quickPrimitive := self quickMethodPrim.
	(primNumber = 0 and: [quickPrimitive > 0]) ifTrue: [
		"if we are a quick method we need to add a byteocde at the beginning. #bytecodes
		takes primitiveBytes into account" 
		primitiveBytes := (ByteArray new: 4) writeStream.
		encoder stream: primitiveBytes.
		encoder genCallPrimitive: quickPrimitive.
	].
	
	header := Smalltalk vm isSpur ifTrue: [ self spurVMHeader: lits size ] ifFalse: [ self v3VMHeader: lits size ].
	^ self compiledMethodWith: trailer header: header literals: lits