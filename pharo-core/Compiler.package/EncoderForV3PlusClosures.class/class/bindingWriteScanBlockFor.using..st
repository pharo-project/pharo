compiled method support
bindingWriteScanBlockFor: litVarIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^ [:b|
		(b = 129 or: [b = 130])
			ifTrue: [scanner followingByte - 192 = litVarIndex]
			ifFalse:
				[b = 132
				 and: [scanner followingByte >= 224
				 and: [scanner thirdByte = litVarIndex]]]]