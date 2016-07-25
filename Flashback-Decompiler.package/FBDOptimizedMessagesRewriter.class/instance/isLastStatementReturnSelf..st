testing
isLastStatementReturnSelf: seq
	^ seq parent isMethod and: [ seq statements last isReturn and: [ seq statements last value isSelf ] ]