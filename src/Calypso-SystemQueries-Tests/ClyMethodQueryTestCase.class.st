Class {
	#name : #ClyMethodQueryTestCase,
	#superclass : #ClyTypedQueryTestCase,
	#instVars : [
		'foundSelectors'
	],
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #testing }
ClyMethodQueryTestCase class >> isAbstract [
	^self == ClyMethodQueryTestCase
]

{ #category : #'methods for tests' }
ClyMethodQueryTestCase >> clyMessageImplementor1 [
]

{ #category : #running }
ClyMethodQueryTestCase >> executeQuery [
	super executeQuery.
	foundSelectors := (resultItems collect: #selector) sorted asArray.
]

{ #category : #running }
ClyMethodQueryTestCase >> setUpEnvironment [
	super setUpEnvironment.
	
	environment system: ClySystemEnvironment currentImage
]

{ #category : #'methods for tests' }
ClyMethodQueryTestCase >> superclassSenderOfMessage1 [
	self clyReferencedMessage1
]

{ #category : #tests }
ClyMethodQueryTestCase >> testIsEmptyFromEmptyMethodScope [
	query scope: (ClyMethodScope emptyIn: environment).
	self assert: query checkEmptyResult
]
