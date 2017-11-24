Class {
	#name : #GTFilterImplementor,
	#superclass : #GTFilterSubstring,
	#category : #'GT-SpotterExtensions-Core'
}

{ #category : #public }
GTFilterImplementor class >> gtListFilter [
	^ GTFilterSubstring new
]

{ #category : #private }
GTFilterImplementor >> applyFilterWithQuery [ 
	super applyFilterWithQuery.
	self filteredItems: (self filteredItems reject: [:each | each isFromTrait]).
]

{ #category : #public }
GTFilterImplementor >> gtListFilter [
	^ GTFilterSubstring new
]

{ #category : #'private-model' }
GTFilterImplementor >> itemFilterNameFor: each [
	self flag: 'this filter wants to filter items by #selector (speed 10x), but dive-in-category wants to filter by #printString. Most filters in dive-in-category wants to filter by #printString, but not all. Others: files, topics, help, ... ?'.
	^ each selector
]
