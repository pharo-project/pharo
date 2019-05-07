Class {
	#name : #AbstractInitializedSlot,
	#superclass : #InstanceVariableSlot,
	#instVars : [
		'default'
	],
	#category : #'Slot-Examples-Base'
}

{ #category : #comparing }
AbstractInitializedSlot >> = other [
	^ super = other and: [default = other default]
]

{ #category : #accessing }
AbstractInitializedSlot >> default [
	^ default
]

{ #category : #accessing }
AbstractInitializedSlot >> default: anObject [
	default := anObject
]

{ #category : #comparing }
AbstractInitializedSlot >> hasSameDefinitionAs: otherSlot [

	^ (super hasSameDefinitionAs: otherSlot) 
		and: [ default = otherSlot default ]

]

{ #category : #comparing }
AbstractInitializedSlot >> hash [
	^super hash bitXor: default hash
]

{ #category : #printing }
AbstractInitializedSlot >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' default: ';
		store: default
]
