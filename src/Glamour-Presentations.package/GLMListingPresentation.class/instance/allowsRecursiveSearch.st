accessing
allowsRecursiveSearch
	^ allowsRecursiveSearch ifNil: [allowsRecursiveSearch := true]