Class {
	#name : 'ClyMethodQueryTest',
	#superclass : 'ClyTypedQueryTest',
	#instVars : [
		'foundSelectors'
	],
	#category : 'Calypso-SystemQueries-Tests-Queries',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Queries'
}

{ #category : 'testing' }
ClyMethodQueryTest class >> isAbstract [
	^self == ClyMethodQueryTest
]

{ #category : 'methods for tests' }
ClyMethodQueryTest >> clyMessageImplementor1 [
]

{ #category : 'running' }
ClyMethodQueryTest >> executeQuery [
	super executeQuery.
	foundSelectors := (resultItems collect: [:each | each selector]) sorted asArray
]

{ #category : 'running' }
ClyMethodQueryTest >> setUpEnvironment [
	super setUpEnvironment.

	environment system: ClySystemEnvironment currentImage
]

{ #category : 'methods for tests' }
ClyMethodQueryTest >> superclassSenderOfMessage1 [
	<ignoreNotImplementedSelectors: #(clyReferencedMessage1)>
	self clyReferencedMessage1
]

{ #category : 'tests' }
ClyMethodQueryTest >> testIsEmptyFromEmptyMethodScope [
	query scope: (ClyMethodScope emptyIn: environment).
	self assert: query checkEmptyResult
]
