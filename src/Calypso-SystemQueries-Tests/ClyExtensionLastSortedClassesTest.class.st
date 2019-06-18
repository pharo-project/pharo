Class {
	#name : #ClyExtensionLastSortedClassesTest,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyExtensionLastSortedClassesTest >> createQueryResult [
	^ClyExtensionLastSortedClasses simple
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testComparisonToSimilarResultButWithWithDifferentBaseResult [

	| result2 |
	queryResult := ClyExtensionLastSortedClasses simple.
	result2 := ClyExtensionLastSortedClasses hierarchical.
	
	self deny: queryResult = result2
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testCreationHierarchicalInstance [

	queryResult := ClyExtensionLastSortedClasses hierarchical.
	
	self assert: queryResult baseQueryResult class equals: ClyHierarchicallySortedClasses 
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testCreationSimpleInstance [

	queryResult := ClyExtensionLastSortedClasses simple.
	
	self assert: queryResult baseQueryResult class equals: ClySortedQueryResult  
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testFillingHierarchicalInstance [
	query := ClyAllClassesQuery from: ClyPackageScope of: ClyClass1FromP1 package in: environment.
	queryResult := ClyExtensionLastSortedClasses hierarchical prepareNewFor: query in: environment..
	
	queryResult buildWith: {
		ClySubclassLOfClass0FromPExtendedByP1. ClySubclassN1OfClass1FromP1.  ClyClass1FromP1}.
	
	self assertItems: {ClyClass1FromP1. 0. ClySubclassN1OfClass1FromP1. 1.
		 ClySubclassLOfClass0FromPExtendedByP1. 0}.
	self assert: (items last isMarkedWith: ClyExtendedClassTag).
	self deny: (items first isMarkedWith: ClyExtendedClassTag).
	self deny: (items second isMarkedWith: ClyExtendedClassTag)
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testFillingSimpleInstance [
	query := ClyAllClassesQuery from: ClyPackageScope of: ClyClass1FromP1 package in: environment.
	queryResult := ClyExtensionLastSortedClasses simple prepareNewFor: query in: environment.

	queryResult buildWith: {
		ClySubclassLOfClass0FromPExtendedByP1. ClySubclassN1OfClass1FromP1.  ClyClass1FromP1}.
	
	self assertItems: {ClyClass1FromP1. 0. ClySubclassN1OfClass1FromP1. 0.
		 ClySubclassLOfClass0FromPExtendedByP1. 0}.
	self assert: (items last isMarkedWith: ClyExtendedClassTag).
	self deny: (items first isMarkedWith: ClyExtendedClassTag).
	self deny: (items second isMarkedWith: ClyExtendedClassTag)
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testFillingWithExtendedClassSide [
	query := ClyAllClassesQuery from: ClyPackageScope of: ClyClass1FromP1 package in: environment.
	queryResult := ClyExtensionLastSortedClasses simple prepareNewFor: query in: environment.
	
	queryResult buildWith: {
		ClySubclassLOfClass0FromPExtendedByP1 class. ClyClass2FromP1.  ClyClass1FromP1}.
	
	self assertItems: {ClyClass1FromP1. 0. ClyClass2FromP1. 0.
		 ClySubclassLOfClass0FromPExtendedByP1 class. 0}.
	self assert: (items last isMarkedWith: ClyExtendedClassTag)
]

{ #category : #tests }
ClyExtensionLastSortedClassesTest >> testHasDefaultBaseQueryResultByDefault [

	self assert: queryResult baseQueryResult class equals: ClySortedQueryResult
]
