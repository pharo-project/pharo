building
lookupOldClass
	oldClass
		ifNil: [ oldClass := builder environment at: builder name ifAbsent: [ nil ] ]