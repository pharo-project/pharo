signaling
on: exception do: handlerAction
	"This method inject new bottom context into process with exception handler. 
	It uses context jump tricks to achieve it"
	| currentContext root newRoot |
	currentContext := self isActiveProcess ifTrue: [ thisContext ] ifFalse: [self suspendedContext].
	root := currentContext bottomContext.
	newRoot := [
			[root insertSender: thisContext.
			currentContext jump] on: exception do: handlerAction.
		Processor terminateActive] asContext.

	self isActiveProcess 
		ifTrue: [ newRoot jump ]
		ifFalse: [ self install: newRoot ]