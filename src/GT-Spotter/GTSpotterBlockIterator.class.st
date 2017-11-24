Class {
	#name : #GTSpotterBlockIterator,
	#superclass : #GTSpotterIterator,
	#instVars : [
		'onDo'
	],
	#category : #'GT-Spotter-Iterators'
}

{ #category : #private }
GTSpotterBlockIterator >> itemDo: aBlock [ 
	self onDo 
		cull: aBlock
		cull: self context 
]

{ #category : #accessing }
GTSpotterBlockIterator >> onDo [
	^ onDo ifNil: [ onDo := [ :anItemBlock :aContext | " do nothing " ] ]
]

{ #category : #accessing }
GTSpotterBlockIterator >> onDo: anObject [
	onDo := anObject
]
