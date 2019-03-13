"
Tests fpr UnlimitedInstanceVariable example
"
Class {
	#name : #BaseSlot,
	#superclass : #InstanceVariableSlot,
	#instVars : [
		'default'
	],
	#category : #'Slot-Examples-Base'
}

{ #category : #comparing }
BaseSlot >> = other [
	^ super = other and: [default = other default]
]

{ #category : #initalize }
BaseSlot >> default [
	^ default
]

{ #category : #initalize }
BaseSlot >> default: anObject [
	default := anObject
]

{ #category : #comparing }
BaseSlot >> hasSameDefinitionAs: otherSlot [

	^ (super hasSameDefinitionAs: otherSlot) 
		and: [ default = otherSlot default ]

]

{ #category : #comparing }
BaseSlot >> hash [
	^super hash bitXor: default hash
]

{ #category : #initalize }
BaseSlot >> initialize: anObject [
	self write: default to: anObject
]

{ #category : #testing }
BaseSlot >> isVisible [
	^ false
]

{ #category : #'meta-object-protocol' }
BaseSlot >> wantsInitalization [
	"if a slot wants to hable initalization, return true here"
	^true
]
