"
This class models definiton of instance Variables (Slots)
"
Class {
	#name : #CDSlotNode,
	#superclass : #CDVariableNode,
	#category : #'ClassParser-Model'
}

{ #category : #transforming }
CDSlotNode >> asSlot [

	"when parsing old style definitions, the node is just the string of all ivars"
	node isLiteralNode ifTrue: [ ^InstanceVariableSlot named: name ]

	"As we controll the creation of the AST, it is safe to evaluate it. The
	only thing that can go wrong is that the class of the slot is not yet loaded,
	we crate an UndefinedSlot for that case".
	^ [ node evaluate ] on: Error do: [ UndefinedSlot named: self name ast: node ]
]

{ #category : #testing }
CDSlotNode >> isClassVariable [
	"To be polymorphic to RB method nodes"
	^false
]

{ #category : #testing }
CDSlotNode >> isInstanceVariable [
	"To be polymorphic to RB method nodes"
	^true
]

{ #category : #testing }
CDSlotNode >> isLiteralVariable [
	"To be polymorphic to RB method nodes"
	^false
]
