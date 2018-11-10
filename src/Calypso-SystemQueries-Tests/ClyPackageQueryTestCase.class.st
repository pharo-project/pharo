Class {
	#name : #ClyPackageQueryTestCase,
	#superclass : #ClyTypedQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyPackageQueryTestCase class >> isAbstract [
	^self = ClyPackageQueryTestCase 
]

{ #category : #tests }
ClyPackageQueryTestCase >> testIsEmptyFromEmptyPackageScope [
	query scope: (ClyPackageScope emptyIn: environment).
	
	self assert: query checkEmptyResult
]
