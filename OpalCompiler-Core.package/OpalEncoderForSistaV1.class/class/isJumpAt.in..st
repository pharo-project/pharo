instruction stream support
isJumpAt: pc in: method
	"Answer whether the bytecode at pc is an (unconditional) jump."

	"	176-183	10110 iii				Jump iii + 1 (i.e., 1 through 8)
	 *	225		11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)
	 *	237		11101101	iiiiiiii		Jump #iiiiiiii (+ Extend B * 256, where bbbbbbbb = sddddddd, e.g. -32768 = i=0, a=0, s=1)"
	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 176 and: [byte <= 183 or: [byte = 237]]