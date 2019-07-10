"
I represent desired selection which should be restored on different data sources.
For example when in the browser user selects particular method and switches to another class then similar method (with same name) should be selected in new class automatically. 
I am created by ClyNavigationView when user manually selects new table items. When new data source is set to view I responsible to restore previous selection of table:
	desiredSelection restoreCurrentSelection: aDataSourceSelection 
Inside this method I ask given selection to find similar items as mine to set them as a new selection:
	aDataSourceSelection restoreDesiredSelectionWith: newItems silently: selectionIsSame.
Last argument indicates that items from new data source are same as selected before. So table should not trigger selection changed event because in fact selection is still same.
To achieve this logic I keep flag that I am the same as current selection from which I was created.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	isSameAsCurrent:		<Boolean>
"
Class {
	#name : #ClyDesiredSelection,
	#superclass : #ClyDataSourceSelection,
	#instVars : [
		'isSameAsCurrent'
	],
	#category : #'Calypso-Browser-DataSource'
}

{ #category : #controlling }
ClyDesiredSelection >> checkItemsOfCurrentSelection: currentSelection [
	| newItems |
	isSameAsCurrent := false.
	newItems := currentSelection items.	

	newItems size = items size ifFalse: [ ^self].
	newItems do: [ :eachNewItem | items do: [ :eachMyItem |
		(eachNewItem isSameAs: eachMyItem) ifFalse: [ ^self ] ] ].

	isSameAsCurrent := true
]

{ #category : #initialization }
ClyDesiredSelection >> initialize [
	super initialize.
	isSameAsCurrent := true
]

{ #category : #accessing }
ClyDesiredSelection >> isSameAsCurrent [
	^ isSameAsCurrent
]

{ #category : #accessing }
ClyDesiredSelection >> isSameAsCurrent: anObject [
	isSameAsCurrent := anObject
]

{ #category : #controlling }
ClyDesiredSelection >> restoreCurrentSelection: currentSelection [
	"We should signal selection change only when previous selection was different 
	and new given data source has no same items.
	We don't want trigger changes when selection was not changed in fact"
	| wasSameAsCurrent newItems currentSelectionIsChanged |
	wasSameAsCurrent := isSameAsCurrent.
	self checkItemsOfCurrentSelection: currentSelection.
	newItems := isSameAsCurrent 
		ifTrue: [ currentSelection items]
		ifFalse: [ currentSelection rootDataSource findItemsSimilarTo: items].
		
	currentSelectionIsChanged := (wasSameAsCurrent & isSameAsCurrent) not.
	currentSelection 
		restoreDesiredSelectionWith: newItems 
		silently: currentSelectionIsChanged not.
	^currentSelectionIsChanged
]

{ #category : #controlling }
ClyDesiredSelection >> restoreCurrentSelectionAfterUpdate: currentSelection [
	"We should signal selection change only when previous selection was different 
	and new given data source has no same items.
	We don't want trigger changes when selection was not changed in fact
	DIFFERENCE from basic #restoreCurrentSelection::
	After update we should not switch selection to similar items.
	After update we should only try to restore exact selection as before.
	So we ask data source to find items exactly with my content
	instead of something similar"
	| wasSameAsCurrent newItems currentSelectionIsChanged |
	wasSameAsCurrent := isSameAsCurrent.
	self checkItemsOfCurrentSelection: currentSelection.
	newItems := isSameAsCurrent 
		ifTrue: [ currentSelection items]
		ifFalse: [ currentSelection rootDataSource findItemsWith: self actualObjects].
		
	currentSelectionIsChanged := (wasSameAsCurrent & isSameAsCurrent) not.
	currentSelection 
		restoreDesiredSelectionWith: newItems 
		silently: currentSelectionIsChanged not.
	^currentSelectionIsChanged
]

{ #category : #controlling }
ClyDesiredSelection >> updateItemsWhichBelongsTo: aDataSource [
	"We override this method by two reasons:
	- desired selection should always keep all items even if some of them was removed from current data source
	- desired selection should never affect actual table selection ofter changes. That's why here we always return false which means that actual selection is not changed"
	| relatedItems updatedItems |
	relatedItems := items select: [ :each | each belongsToDataSource: aDataSource].
	updatedItems := aDataSource itemCursor findItemsWith: (relatedItems collect: #actualObject).

	relatedItems with: updatedItems do: [ :myItem :updatedBrowserItem | 
		updatedBrowserItem ifNotNil: [ myItem updateItemWith: updatedBrowserItem ]].
	
	^false 
]
