Class {
	#name : 'ClyPackageQueryTest',
	#superclass : 'ClyTypedQueryTest',
	#category : 'Calypso-SystemQueries-Tests-Queries',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Queries'
}

{ #category : 'testing' }
ClyPackageQueryTest class >> isAbstract [
	^self = ClyPackageQueryTest
]

{ #category : 'tests' }
ClyPackageQueryTest >> testIsEmptyFromEmptyPackageScope [
	query scope: (ClyPackageScope emptyIn: environment).

	self assert: query checkEmptyResult
]
