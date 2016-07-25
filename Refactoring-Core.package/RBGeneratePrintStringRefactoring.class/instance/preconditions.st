preconditions
preconditions
	^ variables
		inject: RBCondition empty
		into: [ :condition :variable | condition & (RBCondition definesInstanceVariable: variable in: self theClass) ]