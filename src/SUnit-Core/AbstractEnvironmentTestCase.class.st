Class {
	#name : #AbstractEnvironmentTestCase,
	#superclass : #TestCase,
	#instVars : [
		'testingEnvironment'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #accessing }
AbstractEnvironmentTestCase >> environmentOfTest [
	^ testingEnvironment
]

{ #category : #private }
AbstractEnvironmentTestCase >> initialize [
	super initialize.
	testingEnvironment := Smalltalk globals.
]

{ #category : #accessing }
AbstractEnvironmentTestCase >> testingEnvironment: anObject [
	testingEnvironment := anObject
]
