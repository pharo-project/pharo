instruction stream support
isBranchIfFalseAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-false."
	
	
	| byte |
	byte := method at: pc.
	^(byte between: 152 and: 159) or: [byte between: 172 and: 175]