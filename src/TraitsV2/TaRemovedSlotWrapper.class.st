"
When renaming Slots in a trait composition, we want to give the trait a new name but else keep the orginal semantics.

This wrapper does exactly that: as a Slot subclass, it has a name. It forwards anything else to the original slot as defined
in the trait.
"
Class {
	#name : #TaRemovedSlotWrapper,
	#superclass : #Slot,
	#instVars : [
		'originalSlot'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'instance creation' }
TaRemovedSlotWrapper class >> for: aSlot [
	^self new originalSlot: aSlot
]

{ #category : #'code generation' }
TaRemovedSlotWrapper >> emitStore: aMethodBuilder [
	originalSlot emitStore: aMethodBuilder
]

{ #category : #'code generation' }
TaRemovedSlotWrapper >> emitValue: aMethodBuilder [
	originalSlot emitValue: aMethodBuilder
]

{ #category : #accessing }
TaRemovedSlotWrapper >> index [
	^ originalSlot index
]

{ #category : #accessing }
TaRemovedSlotWrapper >> index: anInteger [
	originalSlot index: anInteger
]

{ #category : #testing }
TaRemovedSlotWrapper >> isVirtual [
	^originalSlot isVirtual
]

{ #category : #testing }
TaRemovedSlotWrapper >> isVisible [
	^originalSlot isVisible
]

{ #category : #accessing }
TaRemovedSlotWrapper >> originalSlot [

	^ originalSlot
]

{ #category : #accessing }
TaRemovedSlotWrapper >> originalSlot: anObject [

	originalSlot := anObject
]

{ #category : #printing }
TaRemovedSlotWrapper >> printOn: aStream [
	originalSlot printOn: aStream
]

{ #category : #'meta-object-protocol' }
TaRemovedSlotWrapper >> read: anObject [
	^ originalSlot read: anObject
]

{ #category : #'meta-object-protocol' }
TaRemovedSlotWrapper >> wantsInitialization [
	^ originalSlot wantsInitialization
]

{ #category : #'meta-object-protocol' }
TaRemovedSlotWrapper >> write: aValue to: anObject [
	^ originalSlot write: aValue to: anObject
]
