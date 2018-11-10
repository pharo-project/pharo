Class {
	#name : #ClyAllPackagesTests,
	#superclass : #ClyPackageQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllPackagesTests >> createQuery [
	^ ClyAllPackages from: ClyPackageScope of: Object package in: environment
]

{ #category : #tests }
ClyAllPackagesTests >> testCheckIfEmpty [
	query scope: (ClyPackageScope of: ClyClass1FromP1 package in: environment).
	
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyAllPackagesTests >> testFromSinglePackage [

	self queryFromScope: ClyPackageScope of: ClyClass1FromP1 package.

	self assert: resultItems asSet equals: {ClyClass1FromP1 package} asSet
]
