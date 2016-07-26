class initialization
flushDependents
	DependentsFields keysAndValuesDo:[:key :dep|
		key ifNotNil:[key removeDependent: nil].
	].
	DependentsFields finalizeValues.