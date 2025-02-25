"
Strategy pattern that describes the sorting functionality.

Sorter allSubclasses collect: #kind an OrderedCollection('' 'alphabetical' '' '')
"
Class {
	#name : 'CompletionSorter',
	#superclass : 'Object',
	#instVars : [
		'context'
	],
	#category : 'NECompletion-Sorting',
	#package : 'NECompletion',
	#tag : 'Sorting'
}

{ #category : 'description' }
CompletionSorter class >> kind [
	^ ''
]

{ #category : 'tools registry' }
CompletionSorter class >> register [
	CompletionContext sorterClass: SizeSorter
]

{ #category : 'accessing' }
CompletionSorter >> context [
	^ context
]

{ #category : 'accessing' }
CompletionSorter >> context: anObject [
	context := anObject
]

{ #category : 'sorting' }
CompletionSorter >> sortCompletionList: aList [

	self shouldBeImplemented
]
