utilities
isTestMethod: aMethodOrMethodDefinition
	| selectorString |
	aMethodOrMethodDefinition methodClass isTestCase
		ifFalse: [ ^ false ].
	aMethodOrMethodDefinition numArgs isZero
		ifFalse: [ ^ false ].
	selectorString := aMethodOrMethodDefinition selector asString.

	"for some reason unary selectors starting with 'should' are supposed
	to be treated as test methods"
	^ (selectorString beginsWith: 'test') or: [ selectorString beginsWith: 'should' ]