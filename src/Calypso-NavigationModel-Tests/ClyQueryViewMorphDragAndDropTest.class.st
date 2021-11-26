"
A ClyQueryViewMorphTest is a test class for testing the behavior of ClyQueryViewMorph
"
Class {
	#name : #ClyQueryViewMorphDragAndDropTest,
	#superclass : #TestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #'tests-focus' }
ClyQueryViewMorphDragAndDropTest >> testHasUserBeenFocusingNotLongEnough [
	| view item |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.
	self deny: (view hasUserBeenFocusingOnItem: item).
]

{ #category : #'tests-focus' }
ClyQueryViewMorphDragAndDropTest >> testHasUserBeenFocusingOnSameItem [
	 
	| view item |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.
	view hoverOverDropItemStartTime: (Time hour: 23 minute: 59 second: 59).
	self assert: (view hasUserBeenFocusingOnItem: item).
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldChangeTheTargetIfItemChanged [
	 
	| view item1 initialObject item2 |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	item2 := ClyDataSourceItem new browserItem: 1 ; yourself.
	view userNowFocusesOn: item1.
	initialObject := view targetDropItem.
	view shouldExpand: item2.
	self deny: initialObject equals: view targetDropItem
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldChangeTheTimeIfDragAndDropIsCompleted [
	 
	| view item initialTime |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view hoverOverDropItemStartTime: (Time hour: 23 minute: 59 second: 59).
	initialTime := view hoverOverDropItemStartTime.
	view shouldExpand: item.
	"This is because we've added so much time.
	Point is that it should have reset to time now"
	self assert: initialTime > view hoverOverDropItemStartTime
	
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldChangeTheTimeIfItemChanged [
	 
	| view item1 item2 initialTime |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	item2 := ClyDataSourceItem new browserItem: 1 ; yourself.
	initialTime := view hoverOverDropItemStartTime.
	view shouldExpand: item2.
	self deny: initialTime equals: view hoverOverDropItemStartTime
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldExpandIfDragAndDropIsCompleted [
	| view item initialTime |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.
	view hoverOverDropItemStartTime: (Time hour: 23 minute: 59 second: 59).
	initialTime := view hoverOverDropItemStartTime.
	
	self assert: (view shouldExpand: item).
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldNiltargetIFDragAndDropIsCompleted [
	 
	| view item |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.
	view hoverOverDropItemStartTime: (Time hour: 23 minute: 59 second: 59).
	view shouldExpand: item.
	self assert: view targetDropItem isNil
	
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldNotChangeTheTargetIfItemDidNotChange [
	 
	| view item1 initialObject |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	view userNowFocusesOn: item1.
	initialObject := view targetDropItem.
	view shouldExpand: item1.
	self assert: initialObject equals: view targetDropItem
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldNotChangeTheTimeIfItemDidNotChange [
	 
	| view item1 initialTime |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	view userNowFocusesOn: item1.
	initialTime := view hoverOverDropItemStartTime.
	view shouldExpand: item1.
	self assert: initialTime equals: view hoverOverDropItemStartTime
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldNotExpandIfDragAndDropIsNotCompleted [
	 
	| view item1 initialTime |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	view userNowFocusesOn: item1.
	initialTime := view hoverOverDropItemStartTime.
	self deny: (view shouldExpand: item1)
]

{ #category : #'tests-expand' }
ClyQueryViewMorphDragAndDropTest >> testShouldExpandShouldNotExpandIfItemChanged [
	 
	| view item1 initialObject item2 |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	item2 := ClyDataSourceItem new browserItem: 1 ; yourself.
	view userNowFocusesOn: item1.
	initialObject := view targetDropItem.
	self deny: (view shouldExpand: item2)
]

{ #category : #'tests-focus' }
ClyQueryViewMorphDragAndDropTest >> testUserIsNotFocusingOnDifferentItems [
	 
	| view item1 item2 |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new browserItem: 1; yourself.
	item2 := ClyDataSourceItem new browserItem: 2; yourself.
	view userNowFocusesOn: item1.
	view hoverOverDropItemStartTime: (Time hour: 23 minute: 59 second: 59).
	self deny: (view hasUserBeenFocusingOnItem: item2).
]
