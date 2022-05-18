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
	"for not this only supports instance variable slots. 
	We need to create a slot here using the slot class and initialization message"
	^InstanceVariableSlot named: name
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
