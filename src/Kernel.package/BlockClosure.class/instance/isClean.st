testing
isClean
	"Answer if the receiver does not close-over any variables other than globals, and does
	 not ^-return (does not close over the home context).  Clean blocks are amenable to
	 being created at compile-time."
	self numCopiedValues > 0 ifTrue:
		[^false].
	self abstractBytecodeMessagesDo:
		[:msg|
		(#(	pushReceiver
			pushReceiverVariable: popIntoReceiverVariable: storeIntoReceiverVariable:
			methodReturnConstant: methodReturnReceiver methodReturnTop)
				includes: msg selector) ifTrue:
					[^false]].
	^true

	"clean:"
		"[] isClean"
		"[:a :b| a < b] isClean"
	"unclean"
		"[^nil] isClean"
		"[self class] isClean"
		"| v | v := 0.
		 [v class] isClean"