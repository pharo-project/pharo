control
checkForClosureCopy: receiver arguments: arguments
	"We just saw a closureCopy:copiedValues: message. Check for and construct a following block."

	| savePc jump |
	receiver == constructor codeThisContext ifFalse: [^false].
	savePc := pc.
	(jump := self interpretJump) notNil ifFalse:
		[pc := savePc.
		 ^nil].
	"Definitely a block"
	self doClosureCopyCopiedValues: arguments last "<BraceNode>" elements
		numArgs: arguments first key
		blockSize: jump.
	^true