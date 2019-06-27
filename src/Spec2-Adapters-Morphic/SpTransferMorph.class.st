"
I am a Spec specific subclass of TransferMorph used to initialize the trasnfer morph state correctly with the Spec constraints
"
Class {
	#name : #SpTransferMorph,
	#superclass : #TransferMorph,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #initialization }
SpTransferMorph >> initialize [

	super initialize.

	dragHand := ActiveHand
]

{ #category : #accessing }
SpTransferMorph >> model [
	^ model
]

{ #category : #accessing }
SpTransferMorph >> model: anObject [
	model := anObject
]

{ #category : #accessing }
SpTransferMorph >> passenger: aBlock [
	passenger := aBlock value
]

{ #category : #accessing }
SpTransferMorph >> shouldCopy [

	^ self dragHand shiftPressed
]

{ #category : #accessing }
SpTransferMorph >> source: aBlock [
	source := aBlock value
]
