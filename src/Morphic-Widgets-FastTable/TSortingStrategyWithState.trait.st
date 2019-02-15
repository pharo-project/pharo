"
Description
--------------------

I am a Trait defining the state management of FTAbstractColumnSortingStrategy subclasses using a state to sort the table.

My users should only define a sort function to be used by the state to sort the FastTable.
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	state:			<aSortingState> 	The state of the sorting (ascending, descending, unsorted).

"
Trait {
	#name : #TSortingStrategyWithState,
	#instVars : [
		'state'
	],
	#category : #'Morphic-Widgets-FastTable-Sorting'
}

{ #category : #accessing }
TSortingStrategyWithState >> ascendingSortFunction [
	^ self explicitRequirement
]

{ #category : #initialization }
TSortingStrategyWithState >> initialize [
	super initialize.
	self reset
]

{ #category : #initialization }
TSortingStrategyWithState >> reset [
	self state: (FTUnsortedSortingState context: self)
]

{ #category : #sorting }
TSortingStrategyWithState >> sortDataSource: aDataSource [
	self state sort: aDataSource
]

{ #category : #accessing }
TSortingStrategyWithState >> sortingIcon [
	^ self state sortingIcon
]

{ #category : #accessing }
TSortingStrategyWithState >> state [
	^ state
]

{ #category : #accessing }
TSortingStrategyWithState >> state: anObject [
	state := anObject
]
