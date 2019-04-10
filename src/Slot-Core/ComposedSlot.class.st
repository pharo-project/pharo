"
Slots can be composed. A composition always has one ""real"" slot that knows how to store values and one ore more SlotDecorators. The decorators are chained and delegate reading/writing to the front of the compostion until the finally use the one storage slot to store or read the value.
"
Class {
	#name : #ComposedSlot,
	#superclass : #Slot,
	#instVars : [
		'implementor',
		'next'
	],
	#category : #'Slot-Core-Variables'
}

{ #category : #'instance creation' }
ComposedSlot class >> named: aSymbol [
	^(super named: aSymbol) implementor: (InstanceVariableSlot named: aSymbol).

]

{ #category : #accessing }
ComposedSlot >> baseSlot [
	"the head of the composition is the slot that manages storage"
	^implementor baseSlot
]

{ #category : #accessing }
ComposedSlot >> implementor: anObject [
	implementor := anObject
]

{ #category : #accessing }
ComposedSlot >> index [
	^self baseSlot index
]

{ #category : #accessing }
ComposedSlot >> index: anInteger [
	^self baseSlot index: anInteger
]

{ #category : #testing }
ComposedSlot >> isVirtual [
	"the base slot defines if we are virtual"
	^self baseSlot isVirtual
]

{ #category : #accessing }
ComposedSlot >> next: anObject [
	next := anObject
]

{ #category : #'meta-object-protocol' }
ComposedSlot >> read: anObject [
	^ implementor read: anObject
]

{ #category : #'meta-object-protocol' }
ComposedSlot >> write: aValue to: anObject [
	^implementor write: aValue to: anObject
]
