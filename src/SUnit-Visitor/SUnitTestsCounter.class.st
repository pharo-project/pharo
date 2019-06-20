"
A visitor which count tests in the visitable object provided.
"
Class {
	#name : #SUnitTestsCounter,
	#superclass : #SUnitVisitor,
	#instVars : [
		'count'
	],
	#category : #'SUnit-Visitor'
}

{ #category : #accessing }
SUnitTestsCounter >> count [
	^ count
]

{ #category : #accessing }
SUnitTestsCounter >> count: anObject [
	count := anObject
]

{ #category : #initialization }
SUnitTestsCounter >> initialize [
	super initialize.
	self count: 0
]

{ #category : #visiting }
SUnitTestsCounter >> visitTestCase: aTestCase [
	"Defines the behaviour while visiting a TestCase.
	 This method must be overrided by users of this trait.
	"
	self count: self count + 1
]
