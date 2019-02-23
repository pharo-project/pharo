"
Description
--------------------

I am an abstract class defining the API of a sorting state.

I'll manage the behavior of a aTSortingStrategyWithState subclasses using me that is related to its state.	
	
I follow the State design pattern.
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	context:		<aTSortingStrategyWithState>	The context I am the state of.

"
Class {
	#name : #FTAbstractSortingState,
	#superclass : #Object,
	#instVars : [
		'context'
	],
	#category : #'Morphic-Widgets-FastTable-Sorting'
}

{ #category : #'instance creation' }
FTAbstractSortingState class >> context: aContext [
	^ self new
		context: aContext;
		yourself
]

{ #category : #accessing }
FTAbstractSortingState >> context [
	^ context
]

{ #category : #accessing }
FTAbstractSortingState >> context: anObject [
	context := anObject
]

{ #category : #sorting }
FTAbstractSortingState >> executeSortingOn: aDataSource [
	self subclassResponsibility
]

{ #category : #accessing }
FTAbstractSortingState >> nextState [
	^ self subclassResponsibility
]

{ #category : #sorting }
FTAbstractSortingState >> sort: aDataSource [
	| nextState |
	nextState := self nextState.
	self context state: nextState.
	nextState executeSortingOn: aDataSource
]

{ #category : #accessing }
FTAbstractSortingState >> sortingIcon [
	^ self subclassResponsibility
]
