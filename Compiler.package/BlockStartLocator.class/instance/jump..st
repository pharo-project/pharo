instruction decoding
jump: offset
	"If this jump is around a block answer the size of that block."

	nextJumpIsAroundBlock ifTrue:
		[nextJumpIsAroundBlock := false.
		 ^offset]