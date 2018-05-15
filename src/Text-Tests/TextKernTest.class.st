"
SUnit tests for class TextKern
"
Class {
	#name : #TextKernTest,
	#superclass : #HashAndEqualsTestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextKernTest >> setUp [
	super setUp.
	prototypes
		add: (TextKern kern: 1) 
]
