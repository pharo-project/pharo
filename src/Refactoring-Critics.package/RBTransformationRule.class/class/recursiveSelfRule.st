accessing
recursiveSelfRule
	^ RecursiveSelfRule ifNil: [ self initializeRecursiveSelfRule ]