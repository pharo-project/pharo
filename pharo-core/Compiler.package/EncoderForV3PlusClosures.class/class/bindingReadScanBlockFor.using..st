compiled method support
bindingReadScanBlockFor: litIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^ [:b|
		b >= 64
		and:
			[b <= 95
				ifTrue: [b - 64 = litIndex]
				ifFalse:
					[b = 128
						ifTrue: [scanner followingByte - 192 = litIndex]
						ifFalse:
							[b = 132
							 and: [(scanner followingByte between: 128 and: 159)
							 and: [scanner thirdByte = litIndex]]]]]]