running
setUp
	super setUp.
	nonEmptyDict := MethodDictionary new.
	nonEmptyDict at: #setUp put: (self class >> #setUp).
	nonEmptyDict at: #nonEmpty put: (self class >> #nonEmpty).