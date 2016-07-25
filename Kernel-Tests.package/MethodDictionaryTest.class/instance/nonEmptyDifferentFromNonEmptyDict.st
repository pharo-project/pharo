requirements
nonEmptyDifferentFromNonEmptyDict
	" return a dictionary for which all keys are not included in nonEmptyDict"
	^ MethodDictionary new
		at: #nonEmptyDifferentFromNonEmptyDict put: (self class >> #nonEmptyDifferentFromNonEmptyDict)