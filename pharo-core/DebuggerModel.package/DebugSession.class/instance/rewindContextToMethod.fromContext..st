evaluating
rewindContextToMethod: aMethod fromContext: aContext
	"this method is typically to be used after a hot compilation of a method from the stack.
	in order to return to the context containg the compiled method."
	| ctxt |
	ctxt :=  interruptedProcess popTo: aContext.
	
	ctxt == aContext
		ifFalse: [
			self inform: 'Method saved, but current context unchanged\because of unwind error. Click OK to see error' withCRs ]
		ifTrue: [
			aMethod isQuick ifFalse: [
				interruptedProcess restartTopWith: aMethod.
				self stepToFirstInterestingBytecodeIn: interruptedProcess ] ].
	self updateContextTo:  ctxt.
	
	"Issue 3015 - Hernan"
	self isInterruptedContextATest ifTrue: [ self prepareTestToRunAgain ].