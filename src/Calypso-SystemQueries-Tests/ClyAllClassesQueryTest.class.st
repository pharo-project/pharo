Class {
	#name : #ClyAllClassesQueryTest,
	#superclass : #ClyClassQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllClassesQueryTest >> createQuery [
	^ ClyAllClassesQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyAllClassesQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyClass1FromP1 in: environment).
	
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyAllClassesQueryTest >> testFromSinglePackage [

	self queryFromScope: ClyPackageScope of: ClyClass1FromP1 package.

	self assert: resultItems asSet equals: ClyClass1FromP1 package definedClasses.
	self deny: (resultItems includes: ClySubclassLOfClass0FromPExtendedByP1)
]
