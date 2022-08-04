Class {
	#name : #RTFFakeTestSuperClass,
	#superclass : #TestCase,
	#category : #'RottenTestsFinder-FakeTests'
}

{ #category : #wrappers }
RTFFakeTestSuperClass >> superAssertHelper [
	self assert: true
]

{ #category : #wrappers }
RTFFakeTestSuperClass >> superNotAssertHelper [
	false ifTrue: [ self assert: true ]
]
