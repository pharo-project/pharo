testing
hasUnImplementedAccessors
	| hasGetter hasSetter |
	
	hasGetter := RBCondition canUnderstand: self selectedVariableName asSymbol in: self selectedClass.
	hasSetter := RBCondition canUnderstand: (self selectedVariableName,':' ) asSymbol in: self selectedClass.
	^ (hasGetter not | hasSetter not) check