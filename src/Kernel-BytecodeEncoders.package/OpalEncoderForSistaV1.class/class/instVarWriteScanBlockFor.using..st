compiled method support
instVarWriteScanBlockFor: varIndexCode using: scanner
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."

	"	200-207	11001 iii			Pop and Store Receiver Variable #iii
	*	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	*	240		11110000	iiiiiiii		Pop and Store Receiver Variable #iiiiiii (+ Extend A * 256)
	*	243		11110011	iiiiiiii		Store Receiver Variable #iiiiiii (+ Extend A * 256)"
	| extension |
	extension := 0.
	^[:b| | prevext |
	   prevext := extension.
	   extension := b = 224 ifTrue: [scanner followingByte bitShift: 8] ifFalse: [0].
	   (b >= 200
	    and: [b < 208
	    and: [b - 200 = varIndexCode]])
	   or: [(b = 240 or: [b = 243])
		  and: [scanner followingByte + prevext = varIndexCode]]]