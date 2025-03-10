Class {
	#name : #RBAbstractEnvTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		#testingEnvironment
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #accessing }
RBAbstractEnvTestCase >> environmentOfTest [
	^ testingEnvironment
]

{ #category : #running }
RBAbstractEnvTestCase >> setUp [
	super setUp.
	testingEnvironment := Smalltalk globals
]

{ #category : #accessing }
RBAbstractEnvTestCase >> testingEnvironment: anObject [
	testingEnvironment := anObject
]
