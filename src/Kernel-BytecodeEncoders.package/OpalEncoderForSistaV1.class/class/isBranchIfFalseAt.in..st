instruction stream support
isBranchIfFalseAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-false."

	"	192-199	11000 iii				Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	 *	239		11101111	iiiiiiii		Pop and Jump 0n False #iiiiiiii (+ Extend B * 256, where Extend B >= 0)"
	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 192 and: [byte <= 199 or: [byte = 239]]