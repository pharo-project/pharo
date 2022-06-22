Class {
	#name : #GreenTest,
	#superclass : #TestCase,
	#category : #'TestRunner-Tests-Utilities-Test'
}

{ #category : #tests }
GreenTest >> testPass [

	self skipOnPharoCITestingEnvironment.
	self assert: true
]

{ #category : #tests }
GreenTest >> testPassAgain [

	self skipOnPharoCITestingEnvironment.
	self assert: true
]
