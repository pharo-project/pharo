preconditions
preconditions
	^ self refactorings
		inject: RBCondition empty
		into: [ :result :each | result & each preconditions ]