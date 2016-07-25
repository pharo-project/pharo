testing
hasClassUnImplementedAccessors
	| hasGetter hasSetter name |

	name := self selectedVariableName asString copy.
	name at: 1 put: name first asLowercase .
	
	hasGetter := RBCondition canUnderstand: name asSymbol in: self selectedClass class.
	hasSetter := RBCondition canUnderstand: (name,':' ) asSymbol in: self selectedClass class.
	^ (hasGetter not | hasSetter not) check