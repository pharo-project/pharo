"
A ClyQueryViewMorphTest is a test class for testing the behavior of ClyQueryViewMorph
"
Class {
	#name : 'ClyQueryViewMorphShouldExpandTest',
	#superclass : 'TestCase',
	#category : 'Calypso-SystemTools-FullBrowser-Tests',
	#package : 'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : 'test-init' }
ClyQueryViewMorphShouldExpandTest >> testFirstDragEventShouldNotBeReadyToExpand [

	| view item1 |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	self deny: (view isReadyToExpandForDrop: item1)
]

{ #category : 'tests-focus' }
ClyQueryViewMorphShouldExpandTest >> testHasUserBeenFocusingNotLongEnough [
	| view item |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.
	self deny: (view hasUserBeenFocusingOnItem: item)
]

{ #category : 'tests-focus' }
ClyQueryViewMorphShouldExpandTest >> testHasUserBeenFocusingOnSameItem [

	| view item |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.
	view hoverOverDropItemStartTime: Time microsecondClockValue / 2.
	self assert: (view hasUserBeenFocusingOnItem: item)
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldChangeTheTargetIfItemChanged [

	| view item1 initialObject item2 |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	item2 := ClyDataSourceItem new browserItem: 1 ; yourself.
	view userNowFocusesOn: item1.
	initialObject := view targetDropItem.
	view isReadyToExpandForDrop: item2.
	self deny: initialObject equals: view targetDropItem
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldChangeTheTimeIfITHasBeenLongEnough [

	| view item initialTime |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view hoverOverDropItemStartTime: Time microsecondClockValue * 2.
	initialTime := view hoverOverDropItemStartTime.
	view isReadyToExpandForDrop: item.
	"This is because we've added so much time.
	Point is that it should have reset to time now"
	self assert: initialTime > view hoverOverDropItemStartTime
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldChangeTheTimeIfItemChanged [

	| view item1 item2 initialTime |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	item2 := ClyDataSourceItem new browserItem: 1 ; yourself.
	initialTime := view hoverOverDropItemStartTime.
	view isReadyToExpandForDrop: item2.
	self deny: initialTime equals: view hoverOverDropItemStartTime
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldExpandIfItHasBeenLongEnough [
	| view item initialTime |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.

	initialTime := view hoverOverDropItemStartTime.
	view hoverOverDropItemStartTime: Time microsecondClockValue / 2.
	self assert: (view isReadyToExpandForDrop: item)
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldExpandIfUserHasBeenFocusedLongEnough [

	| view item |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.
	view hoverOverDropItemStartTime: Time microsecondClockValue / 2.
	view isReadyToExpandForDrop: item.
	self assert: view targetDropItem isNil
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldNotAllowToExpandInstantlyLastExpandedItem [
	| view item initialTime |
	view := ClyQueryViewMorph new.
	item := ClyDataSourceItem new.
	view userNowFocusesOn: item.
	view hoverOverDropItemStartTime: Time microsecondClockValue / 2.
	initialTime := view hoverOverDropItemStartTime.
	"This checks that we indeed finalized an expand"
	self assert: (view isReadyToExpandForDrop: item).
	self deny: (view isReadyToExpandForDrop: item)
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldNotChangeTheTargetIfItemDidNotChange [

	| view item1 initialObject |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	view userNowFocusesOn: item1.
	initialObject := view targetDropItem.
	view isReadyToExpandForDrop: item1.
	self assert: initialObject equals: view targetDropItem
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldNotChangeTheTimeIfItemDidNotChange [

	| view item1 initialTime |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	view userNowFocusesOn: item1.
	initialTime := view hoverOverDropItemStartTime.
	view isReadyToExpandForDrop: item1.
	self assert: initialTime equals: view hoverOverDropItemStartTime
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldNotExpandIfItHasNotBeenLongEnough [

	| view item1 initialTime |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	view userNowFocusesOn: item1.
	initialTime := view hoverOverDropItemStartTime.
	self deny: (view isReadyToExpandForDrop: item1)
]

{ #category : 'tests-expand' }
ClyQueryViewMorphShouldExpandTest >> testShouldNotExpandIfItemChanged [

	| view item1 initialObject item2 |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new.
	item2 := ClyDataSourceItem new browserItem: 1 ; yourself.
	view userNowFocusesOn: item1.
	initialObject := view targetDropItem.
	self deny: (view isReadyToExpandForDrop: item2)
]

{ #category : 'tests-focus' }
ClyQueryViewMorphShouldExpandTest >> testUserIstFocusingOnDifferentItems [

	| view item1 item2 |
	view := ClyQueryViewMorph new.
	item1 := ClyDataSourceItem new browserItem: 1; yourself.
	item2 := ClyDataSourceItem new browserItem: 2; yourself.
	view userNowFocusesOn: item1.
	view hoverOverDropItemStartTime: (Time hour: 23 minute: 59 second: 59).
	self deny: (view hasUserBeenFocusingOnItem: item2)
]
