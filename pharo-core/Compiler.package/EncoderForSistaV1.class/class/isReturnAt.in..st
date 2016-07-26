instruction stream support
isReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."
	"	88-91		010110 ii			Return Receiver/true/false/nil
		92			01011100			Return top
		93			01011101			BlockReturn nil
	 *	94			01011110			BlockReturn Top [* return from enclosing block N, N = Extend A, then jump by Ext B ]"
	^(self nonExtensionBytecodeAt: pc in: method) between: 88 and: 94