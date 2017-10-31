self evaluating
isSelfEvaluating

	^self == Transcript or: [ super isSelfEvaluating ]