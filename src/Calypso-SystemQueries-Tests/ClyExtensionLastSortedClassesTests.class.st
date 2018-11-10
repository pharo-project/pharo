Class {
	#name : #ClyExtensionLastSortedClassesTests,
	#superclass : #ClyBrowserQueryResultTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyExtensionLastSortedClassesTests >> createQueryResult [
	^ClyExtensionLastSortedClasses simple
]

{ #category : #tests }
ClyExtensionLastSortedClassesTests >> testComparisonToSimilarResultButWithWithDifferentBaseResult [

	| result2 |
	queryResult := ClyExtensionLastSortedClasses simple.
	result2 := ClyExtensionLastSortedClasses hierarchical.
	
	self deny: queryResult = result2
]

{ #category : #tests }
ClyExtensionLastSortedClassesTests >> testCreationHierarchicalInstance [

	queryResult := ClyExtensionLastSortedClasses hierarchical.
	
	self assert: queryResult baseQueryResult class equals: ClyHierarchicallySortedClasses 
]

{ #category : #tests }
ClyExtensionLastSortedClassesTests >> testCreationSimpleInstance [

	queryResult := ClyExtensionLastSortedClasses simple.
	
	self assert: queryResult baseQueryResult class equals: ClySortedQueryResult  
]

{ #category : #tests }
ClyExtensionLastSortedClassesTests >> testFillingHierarchicalInstance [
	query := ClyAllClasses from: ClyPackageScope of: ClyClass1FromP1 package in: environment.
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
ClyExtensionLastSortedClassesTests >> testFillingSimpleInstance [
	query := ClyAllClasses from: ClyPackageScope of: ClyClass1FromP1 package in: environment.
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
ClyExtensionLastSortedClassesTests >> testFillingWithExtendedClassSide [
	query := ClyAllClasses from: ClyPackageScope of: ClyClass1FromP1 package in: environment.
	queryResult := ClyExtensionLastSortedClasses simple prepareNewFor: query in: environment.
	
	queryResult buildWith: {
		ClySubclassLOfClass0FromPExtendedByP1 class. ClyClass2FromP1.  ClyClass1FromP1}.
	
	self assertItems: {ClyClass1FromP1. 0. ClyClass2FromP1. 0.
		 ClySubclassLOfClass0FromPExtendedByP1 class. 0}.
	self assert: (items last isMarkedWith: ClyExtendedClassTag)
]

{ #category : #tests }
ClyExtensionLastSortedClassesTests >> testHasDefaultBaseQueryResultByDefault [

	self assert: queryResult baseQueryResult class equals: ClySortedQueryResult
]
