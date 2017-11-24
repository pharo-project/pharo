"
I am a Spec specific subclass of TransferMorph used to initialize the trasnfer morph state correctly with the Spec constraints
"
Class {
	#name : #SpecTransferMorph,
	#superclass : #TransferMorph,
	#instVars : [
		'model'
	],
	#category : #'Spec-MorphicAdapters-Support'
}

{ #category : #initialization }
SpecTransferMorph >> initialize [

	super initialize.

	dragHand := ActiveHand
]

{ #category : #accessing }
SpecTransferMorph >> model [
	^ model
]

{ #category : #accessing }
SpecTransferMorph >> model: anObject [
	model := anObject
]

{ #category : #accessing }
SpecTransferMorph >> passenger: aBlock [
	passenger := aBlock value
]

{ #category : #accessing }
SpecTransferMorph >> shouldCopy [

	^ self dragHand shiftPressed
]

{ #category : #accessing }
SpecTransferMorph >> source: aBlock [
	source := aBlock value
]
