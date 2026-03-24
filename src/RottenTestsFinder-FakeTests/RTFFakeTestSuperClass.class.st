"
This class hierarchy is used as data (a TestCase) for RottenTestsFinder test cases.
It should not be checked as a usual test case (critics, release tests), nor being ran.
That's why it inherits from Object. We change the super class to TestCase when needed in RottenTestsFinder tests setUp.
"
Class {
	#name : 'RTFFakeTestSuperClass',
	#superclass : 'TestCase',
	#classVars : [
		'ShouldIgnore'
	],
	#category : 'RottenTestsFinder-FakeTests',
	#package : 'RottenTestsFinder-FakeTests'
}

{ #category : 'asserting' }
RTFFakeTestSuperClass class >> doNotIgnore [ 

	ShouldIgnore := false
]

{ #category : 'asserting' }
RTFFakeTestSuperClass class >> ignore [ 

	ShouldIgnore := true
]

{ #category : 'running' }
RTFFakeTestSuperClass >> setUp [ 

	super setUp.
	self shouldIgnore ifTrue: [ self skip ]
]

{ #category : 'accessing' }
RTFFakeTestSuperClass >> shouldIgnore [

	^ ShouldIgnore ifNil: [ ShouldIgnore := true ]
]

{ #category : 'accessing' }
RTFFakeTestSuperClass >> shouldIgnore: anObject [

	ShouldIgnore := anObject
]

{ #category : 'asserting' }
RTFFakeTestSuperClass >> shouldLogWithEpicea [

	^ false
]

{ #category : 'wrappers' }
RTFFakeTestSuperClass >> superAssertHelper [
	self assert: true
]

{ #category : 'wrappers' }
RTFFakeTestSuperClass >> superNotAssertHelper [
	false ifTrue: [ self assert: true ]
]
