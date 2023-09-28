Class {
	#name : 'ArgumentVariableTest',
	#superclass : 'TestCase',
	#category : 'Slot-Tests-VariablesAndSlots',
	#package : 'Slot-Tests',
	#tag : 'VariablesAndSlots'
}

{ #category : 'tests' }
ArgumentVariableTest >> testDefiningNode [
	| method declaringNode declaringNodeViaVariable|

	method := OrderedCollection >> #do:.
	declaringNode := method ast arguments first.
	declaringNodeViaVariable := method ast variableReadNodes third variable definingNode.
	self assert: declaringNodeViaVariable equals: declaringNode.

	"check block argument"
	declaringNode := method blockNodes first arguments first.
	declaringNodeViaVariable := method ast variableReadNodes fifth variable definingNode.
	self assert: declaringNodeViaVariable equals: declaringNode
]

{ #category : 'tests' }
ArgumentVariableTest >> testUsingMethods [

	| method |
	method := Integer >> #+.
	self
		assert: method temporaryVariables first usingMethods first
		equals: method
]
