Class {
	#name : #ClyExtendingPackagesQueryTest,
	#superclass : #ClyPackageQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyExtendingPackagesQueryTest >> createQuery [
	^ ClyExtendingPackagesQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyExtendingPackagesQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClySubclassLOfClass0FromPExtendedByP1Mock in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ClyClass1FromP1Mock in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyExtendingPackagesQueryTest >> testFromClassWhichIncludesTwoProtocolExtensionsOfSamePackage [

	self queryFromScope: ClyClassScope of: ClyClass9FromPExtendedByP5Mock.

	self assert: resultItems asSet equals: {ClyClass7WithTag1FromP5Mock package} asSet
]

{ #category : #tests }
ClyExtendingPackagesQueryTest >> testFromSingleClass [

	self queryFromScope: ClyClassScope of: ClySubclassLOfClass0FromPExtendedByP1Mock.

	self assert: resultItems asSet equals: {ClyClass1FromP1Mock package} asSet
]

{ #category : #tests }
ClyExtendingPackagesQueryTest >> testFromTwoClasses [

	self queryFromScope: ClyClassScope ofAll: {
		ClyClass9FromPExtendedByP5Mock. ClySubclassLOfClass0FromPExtendedByP1Mock }.

	self assert: resultItems asSet equals: {ClyClass1FromP1Mock package. ClyClass7WithTag1FromP5Mock package} asSet
]
