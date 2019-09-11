"
I am a hidden non-virtual slot that the virtual slots BooleanSlot and PropertySlot store into.

They, when a class is created containing it, makes sure that there is one slot of me defined. When the last virtual slot is removed, I am removed.
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
BaseSlot >> hash [
	^super hash bitXor: default hash
]

{ #category : #initalize }
BaseSlot >> initialize: anObject [
	self write: default copy to: anObject
]

{ #category : #testing }
BaseSlot >> isVisible [
	^ false
]

{ #category : #'meta-object-protocol' }
BaseSlot >> wantsInitialization [
	"we need to call the initialize to set the default value for the base slot"
	^true
]
