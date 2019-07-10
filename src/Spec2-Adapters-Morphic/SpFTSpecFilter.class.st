"
Description
--------------------

I am a filter for Spec lists. I am parametrized by a block and will filter the entities matching the block.

Example 
--------------------

	FTSpecFilter block: [ :each :pattern | each asLowercase includesSubstring: pattern asLowercase ]
"
Class {
	#name : #SpFTSpecFilter,
	#superclass : #FTFilter,
	#instVars : [
		'block'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #'instance creation' }
SpFTSpecFilter class >> block: aBlockClosure [
	^ self new
		block: aBlockClosure;
		yourself
]

{ #category : #accessing }
SpFTSpecFilter >> block [
	^ block
]

{ #category : #accessing }
SpFTSpecFilter >> block: anObject [
	block := anObject
]

{ #category : #matching }
SpFTSpecFilter >> matches: anObject [
	^ self block cull: anObject cull: pattern
]
