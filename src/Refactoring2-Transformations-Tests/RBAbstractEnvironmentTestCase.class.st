Class {
	#name : #RBAbstractEnvironmentTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'testingEnvironment'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #accessing }
RBAbstractEnvironmentTestCase >> environmentOfTest [
	^ testingEnvironment
]

{ #category : #accessing }
RBAbstractEnvironmentTestCase >> setUp [
	super setUp.
	testingEnvironment := Smalltalk globals.
]

{ #category : #accessing }
RBAbstractEnvironmentTestCase >> testingEnvironment: anObject [
	testingEnvironment := anObject
]
