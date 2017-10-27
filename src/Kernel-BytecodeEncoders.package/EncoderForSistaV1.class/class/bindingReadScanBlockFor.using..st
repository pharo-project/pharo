compiled method support
bindingReadScanBlockFor: litVarIndex using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."

	"	16-31		0001 i i i i				Push Literal Variable #iiii
	 *	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	 *	227		11100011	i i i i i i i i	Push Literal Variable #iiiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b| | prevext |
	   prevext := extension.
	   extension := b = 224 ifTrue: [scanner followingByte bitShift: 8] ifFalse: [0].
	   (b < 32 and: [b >= 16 and: [b - 16 = litVarIndex]])
	    or: [b = 227
			and: [scanner followingByte + prevext = litVarIndex]]]