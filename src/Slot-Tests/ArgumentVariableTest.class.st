Class {
	#name : #ArgumentVariableTest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #tests }
ArgumentVariableTest >> testDeclaringNode [
	| method declaringNode declaringNodeViaVariable|
	
	method := OrderedCollection >> #do:.
	declaringNode := method ast arguments first.
	declaringNodeViaVariable := method ast variableReadNodes third variable variable declaringNode.
	self assert: declaringNodeViaVariable equals: declaringNode.
	
	"check block argument"
	declaringNode := method blockNodes first arguments first.
	declaringNodeViaVariable := method ast variableReadNodes fifth variable variable declaringNode.
	self assert: declaringNodeViaVariable equals: declaringNode.
]
