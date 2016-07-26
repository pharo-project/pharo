converting
convertWhileTrueToToByDo: msgNode start: start end: end by: incrValue incrVariable: incrVar assignement: assignement
	| selector arguments loopBlock |
	loopBlock := msgNode arguments last.
	loopBlock arguments: {incrVar}.
	loopBlock statements removeLast.
	incrValue value = 1
		ifTrue: [ 
			selector := #to:do: .
			arguments := {end . loopBlock} ]
		ifFalse: [ 
			selector := #to:by:do: .
			arguments := {end . incrValue . loopBlock} ].
	msgNode
		receiver: start;
		selector: selector;
		arguments: arguments.
	self removeTempFromSequence: msgNode parent temp: incrVar.
	msgNode parent removeNode: assignement