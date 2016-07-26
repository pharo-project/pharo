instruction decoding
blockReturnConstant: cst
	"Return Top Of Stack bytecode."
	scanner pc < blockEnd ifTrue:
		[self doJoin]