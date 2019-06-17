Class {
	#name : #ClyClassQueryTestCase,
	#superclass : #ClyTypedQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyClassQueryTestCase class >> isAbstract [
	^self = ClyClassQueryTestCase
]

{ #category : #tests }
ClyClassQueryTestCase >> testIsEmptyFromEmptyClassScope [

	query scope: (ClyClassScope emptyIn: environment).
	
	self assert: query checkEmptyResult
]
