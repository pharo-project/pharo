Class {
	#name : #ClyMethodVisibilityGroupsTest,
	#superclass : #ClyBrowserQueryResultTest,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyMethodVisibilityGroupsTest >> createQuery [
	^super createQuery 
		scope: (ClyClassScope of: ClyClass1FromP1Mock in: environment)
]

{ #category : #running }
ClyMethodVisibilityGroupsTest >> createQueryResult [
	^ClyMethodVisibilityGroupsResult withHierarchy: ClySubclassHierarchy new 
]

{ #category : #running }
ClyMethodVisibilityGroupsTest >> meaningfullObjectToAssert: resultItem [
	^resultItem actualObject methodQuery scope basisObjects anyOne
]

{ #category : #tests }
ClyMethodVisibilityGroupsTest >> testComparisonWithAnotherHierarchy [
	| queryResult2 |
	queryResult2 := self createQueryResult.
	queryResult2 hierarchy: queryResult hierarchy inverted.

	self deny: queryResult equals: queryResult2
]

{ #category : #tests }
ClyMethodVisibilityGroupsTest >> testDefaultHierarchyShouldExist [

	self assert: (ClyMethodVisibilityGroupsResult defaultHierarchy isKindOf: ClyClassHierarchy)
]

{ #category : #tests }
ClyMethodVisibilityGroupsTest >> testFillingWithSingleClasses [
	| group visibilityLevel |
	queryResult fillWith: {ClyClass2FromP1Mock}.
	self assert: queryResult size equals: 1.
	group := queryResult items first actualObject.
	self assert: group class equals: ClyInheritedMethodGroup.
	self assert: group name equals: ClyClass2FromP1Mock name.
	self
		assert: group methodQuery
		equals: (ClyAllMethodsQuery from: ClyClassScope	of: ClyClass2FromP1Mock in: environment).
	self assert: group visibilityLevels size equals: 1.
	visibilityLevel := group visibilityLevels first.
	self
		assert: visibilityLevel class
		equals: ClySuperclassVisibilityLevel.
	self
		assert: visibilityLevel visibleClassScope
		equals: (ClyClassScope of: ClyClass1FromP1Mock in: environment).
	self
		assert: visibilityLevel extraClassScope
		equals: (ClyClassScope of: ClyClass2FromP1Mock in: environment)
]

{ #category : #tests }
ClyMethodVisibilityGroupsTest >> testFillingWithThreeClasses [

	queryResult fillWith: {ClyClass2FromP1Mock. ClySubclassN1OfClass1FromP1Mock. ClyClass1FromP1Mock}.
	
	self assertItems: { ClyClass1FromP1Mock. 0. ClySubclassN1OfClass1FromP1Mock. 1.
		ClyClass2FromP1Mock. 0}
]
