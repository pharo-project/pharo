Class {
	#name : #ClyMethodVisibilityGroupsTests,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyMethodVisibilityGroupsTests >> createQuery [
	^super createQuery 
		scope: (ClyClassScope of: ClyClass1FromP1 in: environment)
]

{ #category : #running }
ClyMethodVisibilityGroupsTests >> createQueryResult [
	^ClyMethodVisibilityGroups withHierarchy: ClySubclassHierarchy new 
]

{ #category : #running }
ClyMethodVisibilityGroupsTests >> meaningfullObjectToAssert: resultItem [
	^resultItem actualObject methodQuery scope basisObjects anyOne
]

{ #category : #tests }
ClyMethodVisibilityGroupsTests >> testComparisonWithAnotherHierarchy [

	| queryResult2 |
	queryResult2 := self createQueryResult. 
	queryResult2 hierarchy: queryResult hierarchy inverted.
	
	self deny: queryResult = queryResult2
]

{ #category : #tests }
ClyMethodVisibilityGroupsTests >> testDefaultHierarchyShouldExist [

	self assert: (ClyMethodVisibilityGroups defaultHierarchy isKindOf: ClyClassHierarchy)
]

{ #category : #tests }
ClyMethodVisibilityGroupsTests >> testFillingWithSingleClasses [
	| group visibilityLevel |
	queryResult fillWith: {ClyClass2FromP1}.
	self assert: queryResult size equals: 1.
	group := queryResult items first actualObject.
	self assert: group class equals: ClyInheritedMethodGroup.
	self assert: group name equals: ClyClass2FromP1 name.
	self
		assert: group methodQuery
		equals: (ClyAllMethods from: ClyClassScope	of: ClyClass2FromP1 in: environment).
	self assert: group visibilityLevels size equals: 1.
	visibilityLevel := group visibilityLevels first.
	self
		assert: visibilityLevel class
		equals: ClySuperclassVisibilityLevel.
	self
		assert: visibilityLevel visibleClassScope
		equals: (ClyClassScope of: ClyClass1FromP1 in: environment).
	self
		assert: visibilityLevel extraClassScope
		equals: (ClyClassScope of: ClyClass2FromP1 in: environment)
]

{ #category : #tests }
ClyMethodVisibilityGroupsTests >> testFillingWithThreeClasses [

	queryResult fillWith: {ClyClass2FromP1. ClySubclassN1OfClass1FromP1. ClyClass1FromP1}.
	
	self assertItems: { ClyClass1FromP1. 0. ClySubclassN1OfClass1FromP1. 1.
		ClyClass2FromP1. 0}
]
