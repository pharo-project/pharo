Class {
	#name : #ClyAllPackagesQueryTest,
	#superclass : #ClyPackageQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllPackagesQueryTest >> createQuery [
	^ ClyAllPackagesQuery from: ClyPackageScope of: Object package in: environment
]

{ #category : #tests }
ClyAllPackagesQueryTest >> testCheckIfEmpty [
	query scope: (ClyPackageScope of: ClyClass1FromP1 package in: environment).
	
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyAllPackagesQueryTest >> testFromSinglePackage [

	self queryFromScope: ClyPackageScope of: ClyClass1FromP1 package.

	self assert: resultItems asSet equals: {ClyClass1FromP1 package} asSet
]
