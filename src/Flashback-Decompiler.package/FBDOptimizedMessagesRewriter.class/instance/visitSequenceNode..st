visiting
visitSequenceNode: seq
	self analyseSeq: seq .
	super visitSequenceNode: seq.
	(self isLastStatementReturnSelf: seq) ifTrue: [ seq statements removeLast ]