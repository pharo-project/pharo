Class {
	#name : #ClyHierarchicallySortedClassesTest,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyHierarchicallySortedClassesTest >> createQueryResult [
	^ClyHierarchicallySortedClasses new
]

{ #category : #tests }
ClyHierarchicallySortedClassesTest >> testComparisonWithResultWithAnotherHierarchyType [

	| queryResult2 |
	queryResult2 := self createQueryResult.
	queryResult2 hierarchy: #anotherHierarchy.
	
	self deny: queryResult = queryResult2
]

{ #category : #tests }
ClyHierarchicallySortedClassesTest >> testComparisonWithResultWithAnotherSortFunction [

	| queryResult2 |
	queryResult2 := queryResult sortedBy: #anotherSortFunction.
	
	self deny: queryResult = queryResult2.
	self deny: queryResult == queryResult2
]

{ #category : #tests }
ClyHierarchicallySortedClassesTest >> testComparisonWithResultWithSameHierarchyAbdSortFunction [

	| queryResult2 |
	queryResult2 := queryResult sortedBy: queryResult hierarchy sortFunction.
	
	self assert: queryResult = queryResult2.
	self assert: queryResult hash = queryResult2 hash.
]

{ #category : #tests }
ClyHierarchicallySortedClassesTest >> testComparisonWithResultWithSimilarInverseHierarchy [

	| queryResult2 |
	queryResult2 := self createQueryResult.
	queryResult2 := queryResult2 withInverseHierarchy.
	
	self deny: queryResult = queryResult2
]

{ #category : #tests }
ClyHierarchicallySortedClassesTest >> testFillingWithClassAndItGrandSuperclass [
	
	queryResult fillWith: { ClyClass1FromP1. ClySubclassLOfSubclassMFromP3 }.
	
	self assertItems: {ClyClass1FromP1. 0. ClySubclassLOfSubclassMFromP3. 1}
]

{ #category : #tests }
ClyHierarchicallySortedClassesTest >> testFillingWithClassesFromOneHierarchy [

	queryResult fillWith: { ClySubclassN1OfClass1FromP1. ClySubclassLOfSubclassMFromP3. ClyClass1FromP1. ClySubclassN2OfClass1FromP1 }.

	self assertItems: {
		ClyClass1FromP1. 0. ClySubclassLOfSubclassMFromP3. 1. 
		ClySubclassN1OfClass1FromP1. 1. ClySubclassN2OfClass1FromP1. 1 }
]

{ #category : #tests }
ClyHierarchicallySortedClassesTest >> testFillingWithObjectAndTwoProtoObjects [

	queryResult fillWith: { Object. ProtoObject. ProtoObject }.
	
	self assertItems: {ProtoObject. 0. Object. 1}
]

{ #category : #tests }
ClyHierarchicallySortedClassesTest >> testFillingWithTwoSameClasses [

	queryResult fillWith: { ClyClass1FromP1. ClyClass1FromP1 }.

	self assertItems: { ClyClass1FromP1. 0 }
]
