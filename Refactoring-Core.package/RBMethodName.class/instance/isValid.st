testing
isValid
	^ (RBCondition checkMethodName: self selector in: self class) 
		and: [ self selector numArgs = self arguments size ]