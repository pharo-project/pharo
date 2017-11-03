"
I represent two args collator block which must return 1, -1 or 0.

Usually I am created directly from block using sorting messages:
	[:a :b | ] ascending
	[:a :b | ] descending.

Look at SortFunction comments for more details.

Internal Representation and Key Implementation Points.

    Instance Variables
	collatorBlock:		<BlockClosure>	This is the collation function that must return a -1, 0, or 1
"
Class {
	#name : #CollatorBlockFunction,
	#superclass : #SortFunction,
	#instVars : [
		'collatorBlock'
	],
	#category : #SortFunctions-Core
}

{ #category : #'instance creation' }
CollatorBlockFunction class >> usingBlock: twoArgsBlock [
	^self new 
		collatorBlock: twoArgsBlock
]

{ #category : #comparing }
CollatorBlockFunction >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ direction = anObject direction
		and: [ collatorBlock = anObject collatorBlock ]
]

{ #category : #accessing }
CollatorBlockFunction >> collatorBlock [
	^ collatorBlock
]

{ #category : #accessing }
CollatorBlockFunction >> collatorBlock: anObject [
	collatorBlock := anObject
]

{ #category : #comparing }
CollatorBlockFunction >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ direction hash bitXor: collatorBlock hash
]

{ #category : #evaluating }
CollatorBlockFunction >> threeWayCompare: anObject1 with: anObject2 [

	^collatorBlock value: anObject1 value: anObject2 
]
