Class {
	#name : #TestCaseWithEnvironment,
	#superclass : #TestCase,
	#instVars : [
		'testingEnvironment'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #accessing }
TestCaseWithEnvironment >> environmentOfTest [
	^ testingEnvironment
]

{ #category : #private }
TestCaseWithEnvironment >> initialize [ 
	testingEnvironment := Smalltalk globals.
]

{ #category : #accessing }
TestCaseWithEnvironment >> testingEnvironment: anObject [
	testingEnvironment := anObject
]
