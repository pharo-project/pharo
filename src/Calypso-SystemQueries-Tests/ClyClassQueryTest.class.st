Class {
	#name : #ClyClassQueryTest,
	#superclass : #ClyTypedQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyClassQueryTest class >> isAbstract [
	^self = ClyClassQueryTest
]

{ #category : #tests }
ClyClassQueryTest >> testIsEmptyFromEmptyClassScope [

	query scope: (ClyClassScope emptyIn: environment).
	
	self assert: query checkEmptyResult
]
