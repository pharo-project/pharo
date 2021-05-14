Class {
	#name : #ClyPackageExtensionMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyPackageExtensionMethodsQueryTest >> createQuery [
	^ ClyPackageExtensionMethodsQuery
		of: ClyClass1FromP1Mock package
		from: (ClyClassScope of: ClyClass1FromP1Mock in: environment)
]

{ #category : #tests }
ClyPackageExtensionMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope 
		of: ClySubclassLOfClass0FromPExtendedByP1Mock >> #extensionMethodFromSubclassL
		in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: ClyClass9FromPExtendedByP5Mock >> #extensionMethodFromClass9 in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyPackageExtensionMethodsQueryTest >> testComparisonToSimilarQueryButWithDifferentPackage [
	| query2 |
	query package: Object package.
	query2 := self createQuery.
	query2 package: String package.

	self deny: query equals: query2
]

{ #category : #tests }
ClyPackageExtensionMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClass1FromP1Mock >> #tag1Method1. Object >> #printString. 
		ClySubclassLOfClass0FromPExtendedByP1Mock >> #extensionMethodFromSubclassL.
		ClyClass9FromPExtendedByP5Mock >> #extensionMethodFromClass9}.
	
	self assert: foundSelectors equals: #(#extensionMethodFromSubclassL)
]
