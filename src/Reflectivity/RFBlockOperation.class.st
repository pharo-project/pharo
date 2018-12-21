"
I model block creation. on #value, I return the created block
"
Class {
	#name : #RFBlockOperation,
	#superclass : #Object,
	#instVars : [
		'block'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFBlockOperation class >> block: aBlock [
	^self new block: aBlock
]

{ #category : #accessing }
RFBlockOperation >> block [
	^ block
]

{ #category : #accessing }
RFBlockOperation >> block: anObject [
	block := anObject
]

{ #category : #evaluating }
RFBlockOperation >> value [
	^block
]
