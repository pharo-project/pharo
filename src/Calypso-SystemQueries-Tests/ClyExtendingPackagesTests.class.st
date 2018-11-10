Class {
	#name : #ClyExtendingPackagesTests,
	#superclass : #ClyPackageQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyExtendingPackagesTests >> createQuery [
	^ ClyExtendingPackages from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyExtendingPackagesTests >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClySubclassLOfClass0FromPExtendedByP1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ClyClass1FromP1 in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyExtendingPackagesTests >> testFromClassWhichIncludesTwoProtocolExtensionsOfSamePackage [

	self queryFromScope: ClyClassScope of: ClyClass9FromPExtendedByP5.

	self assert: resultItems asSet equals: {ClyClass7WithTag1FromP5 package} asSet
]

{ #category : #tests }
ClyExtendingPackagesTests >> testFromSingleClass [

	self queryFromScope: ClyClassScope of: ClySubclassLOfClass0FromPExtendedByP1.

	self assert: resultItems asSet equals: {ClyClass1FromP1 package} asSet
]

{ #category : #tests }
ClyExtendingPackagesTests >> testFromTwoClasses [

	self queryFromScope: ClyClassScope ofAll: {
		ClyClass9FromPExtendedByP5. ClySubclassLOfClass0FromPExtendedByP1 }.

	self assert: resultItems asSet equals: {ClyClass1FromP1 package. ClyClass7WithTag1FromP5 package} asSet
]
