testing
transitionsToNextSequence
	"Does the current instruction intrinsically know how to transition to the next sequence?"
	^ self isJump or: [self isReturn or: [self isPushClosureCopy ]]