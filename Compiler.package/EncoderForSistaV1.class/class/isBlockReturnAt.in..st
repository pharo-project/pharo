instruction stream support
isBlockReturnAt: pc in: method
	"Answer whether the bytecode at pc is a return from block."
	"	93			01011101			BlockReturn nil
	 *	94			01011110			BlockReturn Top [* return from enclosing block N, N = Extend A, then jump by Ext B ]"
	^(self nonExtensionBytecodeAt: pc in: method) between: 93 and: 94