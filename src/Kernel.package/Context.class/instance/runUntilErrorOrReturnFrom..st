controlling
runUntilErrorOrReturnFrom: aSender 
	"ASSUMES aSender is a sender of self.  Execute self's stack until aSender returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSender returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSender that jumps back to thisContext when evaluated.  We also insert an exception handler under aSender that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error context here topContext |
	here := thisContext.

	"Insert ensure and exception handler contexts under aSender"
	error := nil.
	context := aSender insertSender: (Context
		contextOn: UnhandledError, Halt do: [:ex |
			error ifNil: [
				"this is ugly but it fixes the side-effects of not sending an Unhandled error on Halt"
				error := (ex isKindOf: Halt) ifTrue: [ ex ] ifFalse: [ ex exception ].
				topContext := thisContext.
				ex resumeUnchecked: here jump ]
			ifNotNil: [ ex pass ]]).
	context := context insertSender: (Context
		contextEnsure: [error ifNil: [
				topContext := thisContext.
				here jump]
		]).
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [
		"No error was raised, remove ensure context by stepping until popped"
		[context isDead] whileFalse: [topContext := topContext stepToCallee].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSender terminateTo: context sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].
