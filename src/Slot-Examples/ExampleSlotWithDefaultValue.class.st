"
NOTE: this is an example of what can be done with Slots. It is *not* an example of what *should* be done with Slots.

I am showing how a slot can initialize itself on object creation.

#name => ExampleSlotWithDefaultValue default: 5
"
Class {
	#name : #ExampleSlotWithDefaultValue,
	#superclass : #InstanceVariableSlot,
	#instVars : [
		'default'
	],
	#category : #'Slot-Examples-base'
}

{ #category : #comparing }
ExampleSlotWithDefaultValue >> = other [
	^ super = other and: [default = other default]
]

{ #category : #accessing }
ExampleSlotWithDefaultValue >> default [
	^ default
]

{ #category : #accessing }
ExampleSlotWithDefaultValue >> default: anObject [
	default := anObject
]

{ #category : #comparing }
ExampleSlotWithDefaultValue >> hasSameDefinitionAs: otherSlot [

	^ (super hasSameDefinitionAs: otherSlot) 
		and: [ default = otherSlot default ]

]

{ #category : #comparing }
ExampleSlotWithDefaultValue >> hash [
	^super hash bitXor: default hash
]

{ #category : #initialization }
ExampleSlotWithDefaultValue >> initialize: anObject [
	self write: default to: anObject. 
	
]

{ #category : #printing }
ExampleSlotWithDefaultValue >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' default: ';
		store: default
]

{ #category : #'meta-object-protocol' }
ExampleSlotWithDefaultValue >> wantsInitalization [
	^ true
]
