compiled method support
instVarReadScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^ [:b|
		b < 16
			ifTrue: [b = varIndexCode]
			ifFalse:
				[b = 128
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 64 and: 95)
						 and: [scanner thirdByte = varIndexCode]]]]]