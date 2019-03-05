"
Description
--------------------

I am a filter for Spec lists. I am parametrized by a block and will filter the entities matching the block.

Example 
--------------------

	FTSpecFilter block: [ :each :pattern | each asLowercase includesSubstring: pattern asLowercase ]
"
Class {
	#name : #FTSpecFilter,
	#superclass : #FTFilter,
	#instVars : [
		'block'
	],
	#category : #'Spec-MorphicAdapters-Support'
}

{ #category : #'instance creation' }
FTSpecFilter class >> block: aBlockClosure [
	^ self new
		block: aBlockClosure;
		yourself
]

{ #category : #accessing }
FTSpecFilter >> block [
	^ block
]

{ #category : #accessing }
FTSpecFilter >> block: anObject [
	block := anObject
]

{ #category : #matching }
FTSpecFilter >> matches: anObject [
	^ self block cull: anObject cull: pattern
]
