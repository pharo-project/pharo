"
Pay attention most of the time the sorters are conflicting with the heuristics. 
This addition is on the edge of being removed. 

Strategy pattern that describes the sorting functionality.
Sorter allSubclasses collect: #kind an OrderedCollection('' 'alphabetical' '' '')
"
Class {
	#name : 'CoCompletionSorter',
	#superclass : 'Object',
	#instVars : [
		'context'
	],
	#category : 'NECompletion-Sorting',
	#package : 'NECompletion',
	#tag : 'Sorting'
}

{ #category : 'description' }
CoCompletionSorter class >> kind [
	^ ''
]

{ #category : 'accessing' }
CoCompletionSorter >> context [
	^ context
]

{ #category : 'accessing' }
CoCompletionSorter >> context: anObject [
	context := anObject
]

{ #category : 'sorting' }
CoCompletionSorter >> sortCompletionList: aList [

	self shouldBeImplemented
]
