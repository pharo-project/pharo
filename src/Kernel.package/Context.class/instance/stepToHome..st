system simulation
stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context."

	| home ctxt here error topContext context |
	
	here := thisContext.
	ctxt := self step.
	ctxt = self ifFalse: [ 
		"Insert ensure and exception handler contexts under aSender"
		error := nil.
		context := aContext insertSender: (Context
			contextOn: UnhandledError do: [:ex |
				error ifNil: [
					"this is ugly but it fixes the side-effects of not sending an Unhandled error on Halt"
					error := (ex isKindOf: Halt) ifTrue: [ ex ] ifFalse: [ ex exception ].
					topContext := thisContext.
					ex resumeUnchecked: here jump ]
						ifNotNil: [ ex pass ]]).
	].
	home := aContext home.
	home == ctxt home ifTrue: [ ^{ctxt. nil} ].
	
	[ctxt := ctxt step.
	error ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
			"aContext terminateTo: context sender."  "remove above ensure and handler contexts"
			^ {ctxt. error}].
	
	home == ctxt home] whileFalse: [
		home isDead ifTrue: [^ {ctxt. nil}]
	].

	^ {ctxt. nil}