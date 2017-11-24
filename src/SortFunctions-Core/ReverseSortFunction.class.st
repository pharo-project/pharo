"
A ReverseSortFunction wraps over another SortFunction so as to sort in reverse order.
"
Class {
	#name : #ReverseSortFunction,
	#superclass : #ComposedSortFunction,
	#category : #'SortFunctions-Core'
}

{ #category : #evaluating }
ReverseSortFunction >> collate: anObject with: another [
	^(baseSortFunction collate: anObject with: another) negated
]

{ #category : #converting }
ReverseSortFunction >> reversed [
	^baseSortFunction
]

{ #category : #converting }
ReverseSortFunction >> undefinedFirst [
	"apply on the original"
	^baseSortFunction undefinedLast reversed
]

{ #category : #converting }
ReverseSortFunction >> undefinedLast [
	"apply on the original"
	^baseSortFunction undefinedFirst reversed
]
