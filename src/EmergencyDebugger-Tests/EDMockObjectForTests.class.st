"
I implement method to build execution stacks and errors to test the Ed debugger
"
Class {
	#name : #EDMockObjectForTests,
	#superclass : #Object,
	#category : #'EmergencyDebugger-Tests'
}

{ #category : #'helper methods' }
EDMockObjectForTests >> m: i [
	i = 1 ifTrue:[^1].
	^i + (self m: i - 1)
]
