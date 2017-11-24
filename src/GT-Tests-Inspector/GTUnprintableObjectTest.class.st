"
GTUnprintableObjectTest is a test that checks that print string does not trigger an error in the inspector.
"
Class {
	#name : #GTUnprintableObjectTest,
	#superclass : #GLMMorphicTest,
	#category : #'GT-Tests-Inspector'
}

{ #category : #printing }
GTUnprintableObjectTest >> printString [
	"Simulate an error in printString"
	1/0
]

{ #category : #tests }
GTUnprintableObjectTest >> testInspectDoesNotCrash [
	window := GTInspector openOn: self class new 
]
