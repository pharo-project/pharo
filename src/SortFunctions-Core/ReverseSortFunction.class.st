"
A ReverseSortFunction wraps over another SortFunction so as to sort in reverse order.
"
Class {
	#name : #ReverseSortFunction,
	#superclass : #ComposedSortFunction,
	#category : #SortFunctions-Core
}

{ #category : #evaluating }
ReverseSortFunction >> collate: anObject with: another [
	^(collator collate: anObject with: another) negated
]

{ #category : #converting }
ReverseSortFunction >> inverted [
	^collator
]

{ #category : #converting }
ReverseSortFunction >> undefinedFirst [
	"apply on the original"
	^collator undefinedLast inverted
]

{ #category : #converting }
ReverseSortFunction >> undefinedLast [
	"apply on the original"
	^collator undefinedFirst inverted
]
