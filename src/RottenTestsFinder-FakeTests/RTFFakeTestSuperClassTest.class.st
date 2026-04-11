"
This class hierarchy is used as data (a TestCase) for RottenTestsFinder test cases.
It should not be checked as a usual test case (critics, release tests), nor being ran.
That's why it inherits from Object. We change the super class to TestCase when needed in RottenTestsFinder tests setUp.
"
Class {
	#name : 'RTFFakeTestSuperClassTest',
	#superclass : 'TestCase',
	#classVars : [
		'ShouldIgnore'
	],
	#category : 'RottenTestsFinder-FakeTests',
	#package : 'RottenTestsFinder-FakeTests'
}

{ #category : 'asserting' }
RTFFakeTestSuperClassTest class >> doNotIgnore [ 

	ShouldIgnore := false
]

{ #category : 'asserting' }
RTFFakeTestSuperClassTest class >> ignore [ 

	ShouldIgnore := true
]

{ #category : 'running' }
RTFFakeTestSuperClassTest >> setUp [ 

	super setUp.
	self shouldIgnore ifTrue: [ self skip ]
]

{ #category : 'accessing' }
RTFFakeTestSuperClassTest >> shouldIgnore [

	^ ShouldIgnore ifNil: [ ShouldIgnore := true ]
]

{ #category : 'accessing' }
RTFFakeTestSuperClassTest >> shouldIgnore: anObject [

	ShouldIgnore := anObject
]

{ #category : 'asserting' }
RTFFakeTestSuperClassTest >> shouldLogWithEpicea [

	^ false
]

{ #category : 'wrappers' }
RTFFakeTestSuperClassTest >> superAssertHelper [
	self assert: true
]

{ #category : 'wrappers' }
RTFFakeTestSuperClassTest >> superNotAssertHelper [
	false ifTrue: [ self assert: true ]
]
