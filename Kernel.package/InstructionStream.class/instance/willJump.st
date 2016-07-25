testing
willJump
	"unconditionally"

	^ self method encoderClass isJumpAt: pc in: self method
	