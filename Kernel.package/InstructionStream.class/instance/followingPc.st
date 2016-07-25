scanning
followingPc
	"Answer the pc of the following bytecode."

	^self nextPc: (self method at: pc)