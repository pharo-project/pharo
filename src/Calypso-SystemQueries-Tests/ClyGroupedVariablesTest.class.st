Class {
	#name : #ClyGroupedVariablesTest,
	#superclass : #ClyBrowserQueryResultTest,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #testing }
ClyGroupedVariablesTest class >> isAbstract [
	^self = ClyGroupedVariablesTest 
]

{ #category : #helpers }
ClyGroupedVariablesTest >> assertVars: varNames [

	| vars i var |
	vars := items 
		select: [ :each | each actualObject isKindOf: ClyVariableMethodGroup ].
	self assert: vars size equals: varNames size.
	i := 1.
	varNames do: [ :expectedVar |
		var := vars at: i.
		self assert: var actualObject name equals: expectedVar.
		i := i + 1]
]

{ #category : #running }
ClyGroupedVariablesTest >> createQuery [
	"ClyGroupedVariables should build variable method groups using query scope.
	Scope is required to get methods from built groups which are visible only from these classes
	and not just from classes which define given variables"

	| scope |
	scope := ClyInstanceSideScope ofAll: {Object. Array. String} in: environment.
	^ ClyAllClassesQuery from: scope
]

{ #category : #helpers }
ClyGroupedVariablesTest >> meaningfullObjectToAssert: aBrowserItem [
	^aBrowserItem actualObject class
]

{ #category : #tests }
ClyGroupedVariablesTest >> testComparisonWithAnotherHierarchy [
	| queryResult2 |
	queryResult2 := self createQueryResult.
	queryResult2 hierarchy: queryResult hierarchy inverted.

	self deny: queryResult equals: queryResult2
]
