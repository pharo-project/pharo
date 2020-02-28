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

{ #category : #running }
AbstractEnvironmentTestCase >> setUp [
	super setUp.
	testingEnvironment := Smalltalk globals.
]

{ #category : #accessing }
AbstractEnvironmentTestCase >> testingEnvironment: anObject [
	testingEnvironment := anObject
]
