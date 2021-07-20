Class {
	#name : #ClyExtensionLastSortedClassesTest,
	#superclass : #ClyBrowserQueryResultTest,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyExtensionLastSortedClassesTest >> createQueryResult [
	^ClyExtensionLastSortedClassResult simple
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testComparisonToSimilarResultButWithWithDifferentBaseResult [
	| result2 |
	queryResult := ClyExtensionLastSortedClassResult simple.
	result2 := ClyExtensionLastSortedClassResult hierarchical.

	self deny: queryResult equals: result2
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testCreationHierarchicalInstance [

	queryResult := ClyExtensionLastSortedClassResult hierarchical.
	
	self assert: queryResult baseQueryResult class equals: ClyHierarchicallySortedClassesResult 
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testCreationSimpleInstance [

	queryResult := ClyExtensionLastSortedClassResult simple.
	
	self assert: queryResult baseQueryResult class equals: ClySortedQueryResult  
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testFillingHierarchicalInstance [
	query := ClyAllClassesQuery from: ClyPackageScope of: ClyClass1FromP1Mock package in: environment.
	queryResult := ClyExtensionLastSortedClassResult hierarchical prepareNewFor: query in: environment..
	
	queryResult buildWith: {
		ClySubclassLOfClass0FromPExtendedByP1Mock. ClySubclassN1OfClass1FromP1Mock.  ClyClass1FromP1Mock}.
	
	self assertItems: {ClyClass1FromP1Mock. 0. ClySubclassN1OfClass1FromP1Mock. 1.
		 ClySubclassLOfClass0FromPExtendedByP1Mock. 0}.
	self assert: (items last isMarkedWith: ClyExtendedClassTag).
	self deny: (items first isMarkedWith: ClyExtendedClassTag).
	self deny: (items second isMarkedWith: ClyExtendedClassTag)
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testFillingSimpleInstance [
	query := ClyAllClassesQuery from: ClyPackageScope of: ClyClass1FromP1Mock package in: environment.
	queryResult := ClyExtensionLastSortedClassResult simple prepareNewFor: query in: environment.

	queryResult buildWith: {
		ClySubclassLOfClass0FromPExtendedByP1Mock. ClySubclassN1OfClass1FromP1Mock.  ClyClass1FromP1Mock}.
	
	self assertItems: {ClyClass1FromP1Mock. 0. ClySubclassN1OfClass1FromP1Mock. 0.
		 ClySubclassLOfClass0FromPExtendedByP1Mock. 0}.
	self assert: (items last isMarkedWith: ClyExtendedClassTag).
	self deny: (items first isMarkedWith: ClyExtendedClassTag).
	self deny: (items second isMarkedWith: ClyExtendedClassTag)
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testFillingWithExtendedClassSide [
	query := ClyAllClassesQuery from: ClyPackageScope of: ClyClass1FromP1Mock package in: environment.
	queryResult := ClyExtensionLastSortedClassResult simple prepareNewFor: query in: environment.
	
	queryResult buildWith: {
		ClySubclassLOfClass0FromPExtendedByP1Mock class. ClyClass2FromP1Mock.  ClyClass1FromP1Mock}.
	
	self assertItems: {ClyClass1FromP1Mock. 0. ClyClass2FromP1Mock. 0.
		 ClySubclassLOfClass0FromPExtendedByP1Mock class. 0}.
	self assert: (items last isMarkedWith: ClyExtendedClassTag)
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testHasDefaultBaseQueryResultByDefault [

	self assert: queryResult baseQueryResult class equals: ClySortedQueryResult
]
