instruction stream support
isBranchIfTrueAt: pc in: method
	"Answer whether the bytecode at pc is a conditional branch-if-true."
	
	| byte |
	byte := method at: pc.
	^ byte between: 168 and: 171