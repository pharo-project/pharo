Class {
	#name : #ClyExtendingPackagesQueryTest,
	#superclass : #ClyPackageQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyExtendingPackagesQueryTest >> createQuery [
	^ ClyExtendingPackagesQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyExtendingPackagesQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClySubclassLOfClass0FromPExtendedByP1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ClyClass1FromP1 in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyExtendingPackagesQueryTest >> testFromClassWhichIncludesTwoProtocolExtensionsOfSamePackage [

	self queryFromScope: ClyClassScope of: ClyClass9FromPExtendedByP5.

	self assert: resultItems asSet equals: {ClyClass7WithTag1FromP5 package} asSet
]

{ #category : #tests }
ClyExtendingPackagesQueryTest >> testFromSingleClass [

	self queryFromScope: ClyClassScope of: ClySubclassLOfClass0FromPExtendedByP1.

	self assert: resultItems asSet equals: {ClyClass1FromP1 package} asSet
]

{ #category : #tests }
ClyExtendingPackagesQueryTest >> testFromTwoClasses [

	self queryFromScope: ClyClassScope ofAll: {
		ClyClass9FromPExtendedByP5. ClySubclassLOfClass0FromPExtendedByP1 }.

	self assert: resultItems asSet equals: {ClyClass1FromP1 package. ClyClass7WithTag1FromP5 package} asSet
]
