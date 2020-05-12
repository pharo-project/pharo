"
I implement method to build execution stacks and errors to test the Ed debugger
"
Class {
	#name : #EDMockObjectForTests,
	#superclass : #Object,
	#category : #'EmergencyDebugger-Tests'
}

{ #category : #'as yet unclassified' }
EDMockObjectForTests >> m [
	self m1
]

{ #category : #'as yet unclassified' }
EDMockObjectForTests >> m1 [
	self m2
]

{ #category : #'as yet unclassified' }
EDMockObjectForTests >> m2 [
	^1
]

{ #category : #'as yet unclassified' }
EDMockObjectForTests >> m4 [
	|i|
	i := 1.
	^i squared
]

{ #category : #'as yet unclassified' }
EDMockObjectForTests >> m: i [
	i = 1 ifTrue:[^1].
	^i + (self m: i - 1)
]
