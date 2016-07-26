testing
isEvaluatedFirst
	"Return true if we are the first thing evaluated in this statement."

	^parent isNil or: [parent isSequence or: [parent evaluatedFirst: self]]