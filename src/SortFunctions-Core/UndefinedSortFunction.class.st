"
An  UndefinedSortFunction is a specialization usefull for sorting undefined objects (nil), either first or last according to direction.
The non nil objects are sorted according to the baseSortFunction defined in superclass.

instance variables:
	direction	<Integer>	-1 for sorting nil first, 1 for sorting nil last
"
Class {
	#name : #UndefinedSortFunction,
	#superclass : #ComposedSortFunction,
	#instVars : [
		'direction'
	],
	#category : #'SortFunctions-Core'
}

{ #category : #comparing }
UndefinedSortFunction >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ baseSortFunction = anObject baseSortFunction
		and: [ direction = anObject direction ]
]

{ #category : #evaluating }
UndefinedSortFunction >> collate: anObject with: another [
	anObject ifNil: [^another ifNil: [0] ifNotNil: [direction]].
	another ifNil: [^direction negated].
	^baseSortFunction collate: anObject with: another
]

{ #category : #accessing }
UndefinedSortFunction >> direction [
	^direction
]

{ #category : #comparing }
UndefinedSortFunction >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: direction hash
]

{ #category : #initialization }
UndefinedSortFunction >> initialize [
	super initialize.
	direction := -1
]

{ #category : #converting }
UndefinedSortFunction >> undefinedFirst [
	direction := -1
]

{ #category : #converting }
UndefinedSortFunction >> undefinedLast [
	direction := 1
]
