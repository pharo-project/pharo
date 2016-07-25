instruction stream support
isJumpAt: pc in: method
	"Answer whether the bytecode at pc is an (unconditional) jump."

	| byte |
	byte := method at: pc.
	^ (byte between: 144 and: 151) or: [byte between: 160 and: 167]