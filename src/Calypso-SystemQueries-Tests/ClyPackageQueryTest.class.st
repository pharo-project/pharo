Class {
	#name : #ClyPackageQueryTest,
	#superclass : #ClyTypedQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyPackageQueryTest class >> isAbstract [
	^self = ClyPackageQueryTest 
]

{ #category : #tests }
ClyPackageQueryTest >> testIsEmptyFromEmptyPackageScope [
	query scope: (ClyPackageScope emptyIn: environment).
	
	self assert: query checkEmptyResult
]
