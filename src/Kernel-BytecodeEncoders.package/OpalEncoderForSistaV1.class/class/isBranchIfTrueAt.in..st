instruction stream support
isBranchIfTrueAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-true."

	"	184-191	10111 iii				Pop and Jump 0n True iii +1 (i.e., 1 through 8)
	 *	238		11101110	iiiiiiii		Pop and Jump 0n True #iiiiiiii (+ Extend B * 256, where Extend B >= 0))"
	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 184 and: [byte <= 191 or: [byte = 238]]