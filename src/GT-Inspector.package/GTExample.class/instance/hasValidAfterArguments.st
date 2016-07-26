testing
hasValidAfterArguments
	^ self after isNil
		or: [ self after hasValidArguments
			and: [ (self method variableNodes collect: [ :e | e name ]) includesAll: self after arguments ] ]