"
I implement a filter for fast table. The filter (filterBlock) is a BlockClosure with two arguments. The first argument is a filtered item, the second argument is a string pattern. The block has to return true if the item should be displayed; false otherwise.
"
Class {
	#name : #GLMFilterBlock,
	#superclass : #FTFilter,
	#instVars : [
		'filterBlock'
	],
	#category : #'Glamour-FastTable-Model'
}

{ #category : #'instance creation' }
GLMFilterBlock class >> block: aBlockClosure [
	^ self new filterBlock: aBlockClosure; yourself
]

{ #category : #accessing }
GLMFilterBlock >> filterBlock [
	^ filterBlock
]

{ #category : #accessing }
GLMFilterBlock >> filterBlock: anObject [
	"Two argument block: 
		- filtered item
		- string pattern.
	The block should return boolean value."
	filterBlock := anObject
]

{ #category : #testing }
GLMFilterBlock >> matches: anObject [
	"Return true if anObject should be displayed; false otherwise."
	self filterBlock ifNil: [ ^ true ].
	^ self filterBlock valueWithEnoughArguments: { anObject. pattern }
]
