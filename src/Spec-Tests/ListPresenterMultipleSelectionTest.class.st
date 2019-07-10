Class {
	#name : #ListPresenterMultipleSelectionTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
ListPresenterMultipleSelectionTest >> classToTest [
	^ ListPresenter
]

{ #category : #running }
ListPresenterMultipleSelectionTest >> setUp [

	super setUp.
	window := presenter
		beMultipleSelection;
		items: #(10 20 30);
		openWithSpec.
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectAllRaisesSelectionEventOnce [
	"Because it does nothing in single selection mode"
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter selectAll.

	self assert: events equals: 1
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectAllSelectsAllItems [

	presenter selectAll.
	self assert: presenter selection selectedItems asSet equals: presenter model items asSet
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexAddsIndexToSelectedIndexList [

	presenter selectIndex: 1.
	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexAddsItemToSelectedItemList [

	presenter selectIndex: 1.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexOutsideRangeHasNoSelectedIndexes [

	presenter selectIndex: 4.
	self assert: presenter selection selectedIndexes isEmpty
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexOutsideRangeHasNoSelectedItems [

	presenter selectIndex: 4.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexOutsideRangeIsEmpty [

	presenter selectIndex: 4.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexThenSelectOutsideRangeKeepsFirstElement [

	presenter selectIndex: 1.
	presenter selectIndex: 50.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexThenSelectOutsideRangeKeepsFirstIndex [

	presenter selectIndex: 1.
	presenter selectIndex: 50.
	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexThenSelectOutsideRangeKeepsSingleSelectedItem [

	presenter selectIndex: 1.
	presenter selectIndex: 50.
	self assert: presenter selection selectedItems size equals: 1
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexTwiceAddsIndexToSelectedIndexListOnlyOnce [

	presenter
		selectIndex: 1;
		selectIndex: 1.
	self assert: presenter selection selectedIndexes asArray equals: #(1)
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexesAddsIndexesToSelectedIndexList [
	presenter selectIndexes: {1 . 2}.
	self assert: (presenter selection includesIndexes: {1 . 2})
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexesAddsItemsToSelectedItemList [
	presenter selectIndexes: {1 . 2}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexesOutsideRangeHasNoSelectedIndexes [
	presenter selectIndexes: {4 . 5}.
	self assert: presenter selection selectedIndexes isEmpty
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexesOutsideRangeHasNoSelectedItems [
	presenter selectIndexes: {4 . 5}.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexesOutsideRangeIsEmpty [
	presenter selectIndexes: {4 . 5}.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexesThenSelectOutsideRangeKeepsElements [
	presenter selectIndexes: {1 . 2}.
	presenter selectIndexes: {50 . 60}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexesThenSelectOutsideRangeKeepsIndexes [
	presenter selectIndexes: {1 . 2}.
	presenter selectIndexes: {50 . 60}.
	self assert: (presenter selection includesIndexes: {1 . 2})
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectIndexesTwiceAddsIndexesToSelectedIndexListOnlyOnce [
	presenter
		selectIndexes: {1 . 2};
		selectIndexes: {1 . 2}.
	self assertCollection: presenter selection selectedIndexes hasSameElements: {1 . 2}
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemAddsIndexToSelectedIndexList [

	presenter selectItem: 10.
	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemAddsItemToSelectedItemList [

	presenter selectItem: 10.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemOutsideRangeHasNoSelectedIndexes [

	presenter selectItem: 400.
	self assert: presenter selection selectedIndexes isEmpty
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemOutsideRangeHasNoSelectedItems [

	presenter selectItem: 400.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemOutsideRangeIsEmpty [

	presenter selectItem: 4000.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsFirstElement [

	presenter selectItem: 10.
	presenter selectItem: 5000.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsFirstIndex [

	presenter selectItem: 10.
	presenter selectItem: 500.
	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsSingleSelectedItem [

	presenter selectItem: 10.
	presenter selectItem: 3000.
	self assert: presenter selection selectedItems size equals: 1
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemsAddsIndexesToSelectedIndexList [
	presenter selectItems: {10 . 20}.
	self assert: (presenter selection includesIndexes: #(1 2))
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemsAddsItemsToSelectedItemList [
	presenter selectItems: {10 . 20}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemsOutsideRangeHasNoSelectedIndexes [
	presenter selectItems: {300 . 400}.
	self assert: presenter selection selectedIndexes isEmpty
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemsOutsideRangeHasNoSelectedItems [
	presenter selectItems: {300 . 400}.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemsOutsideRangeIsEmpty [
	presenter selectItems: {4000 . 5000}.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemsThenSelectOutsideRangeKeepsElements [
	presenter selectItems: {10 . 20}.
	presenter selectItems: {4000 . 5000}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectItemsThenSelectOutsideRangeKeepsIndexes [
	presenter selectItems: {10 . 20}.
	presenter selectItems: {500 . 600}.
	self assert: (presenter selection includesIndexes: {1 . 2})
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectMultipleIndexesAddsAllToSelectedIndexList [

	presenter selectIndex: 1.
	presenter selectIndex: 3.
	self assert: (presenter selection includesIndex: 1).
	self assert: (presenter selection includesIndex: 3).
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectMultipleIndexesAddsAllToSelectedItemList [

	presenter selectIndex: 1.
	presenter selectIndex: 3.
	self assert: (presenter selection includesItem: 10).
	self assert: (presenter selection includesItem: 30).
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSelectMultipleIndexesRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter	selectIndex: 1.
	presenter	selectIndex: 3.

	self assert: events equals: 2
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectMultipleItemAddsAllToSelectedIndexList [

	presenter selectItem: 10.
	presenter selectItem: 30.
	self assert: (presenter selection includesIndex: 1).
	self assert: (presenter selection includesIndex: 3).
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectMultipleItemsAddsAllToSelectedItemList [

	presenter selectItem: 10.
	presenter selectItem: 30.
	self assert: (presenter selection includesItem: 10).
	self assert: (presenter selection includesItem: 30).
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSelectMultipleItemsRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter selectItem: 10.
	presenter selectItem: 30.

	self assert: events equals: 2
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSetSelectIndexOutsideRangeDoesNotModifySelection [

	presenter whenSelectionChangedDo: [ :selection | self fail ].
	
	presenter selectIndex: 4.
	"If we arrive here and the test did not fail, we succeeded"
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSetSelectIndexRaisesSelectionChangeEventWithSelectedIndex [
	| selectedIndexes |
	presenter
		whenSelectionChangedDo: [ :selection | selectedIndexes := selection selectedIndexes ].
	presenter selectIndex: 1.
	self assert: (selectedIndexes includes: 1)
]

{ #category : #'tests-select-index' }
ListPresenterMultipleSelectionTest >> testSetSelectIndexRaisesSelectionChangeEventWithSelectedItem [
	| selectedItems |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItems := selection selectedItems ].
	presenter selectIndex: 1.
	self assert: (selectedItems includes: 10)
]

{ #category : #'tests-select-item' }
ListPresenterMultipleSelectionTest >> testSetSelectItemOutsideRangeDoesNotModifySelection [

	presenter whenSelectionChangedDo: [ :selection | self fail ].
	
	presenter selectItem: 100.
	"If we arrive here and the test did not fail, we succeeded"
]

{ #category : #'tests-unselect-index' }
ListPresenterMultipleSelectionTest >> testUnselectAllRaisesSelectionEventOnce [
	"Because it does nothing in single selection mode"

	| events |
	events := 0.

	"Be sure there is one element selected at least to make selection change"
	presenter selectIndex: 1.
	presenter
		whenSelectionChangedDo: [ :selection | events := events + 1 ].
	presenter unselectAll.
	self assert: events equals: 1
]

{ #category : #'tests-unselect-index' }
ListPresenterMultipleSelectionTest >> testUnselectAllUnselectsall [

	presenter
		selectIndex: 1;
		unselectAll.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-index' }
ListPresenterMultipleSelectionTest >> testUnselectSelectedIndexRaisesSelectionEventOnce [
	
	| counter |
	counter := 0.
	presenter
		selectIndex: 1;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectIndex: 1.
	self assert: counter equals: 1
]

{ #category : #'tests-unselect-index' }
ListPresenterMultipleSelectionTest >> testUnselectSelectedIndexRemovesItFromSelectionList [

	presenter
		selectIndex: 1;
		unselectIndex: 1.
	self assert: (presenter selection isEmpty)
]

{ #category : #'tests-unselect-item' }
ListPresenterMultipleSelectionTest >> testUnselectSelectedItemRaisesSelectionEventOnce [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 10.
	self assert: counter equals: 1
]

{ #category : #'tests-unselect-item' }
ListPresenterMultipleSelectionTest >> testUnselectSelectedItemRemovesItFromSelectionList [

	presenter
		selectItem: 10;
		unselectItem: 10.
	self assert: (presenter selection isEmpty)
]

{ #category : #'tests-unselect-index' }
ListPresenterMultipleSelectionTest >> testUnselectUnselectedIndexKeepsSelectionList [

	presenter
		selectIndex: 1;
		unselectIndex: 2.
	self assert: presenter selection selectedIndexes asArray equals: #(1)
]

{ #category : #'tests-unselect-index' }
ListPresenterMultipleSelectionTest >> testUnselectUnselectedIndexRaisesNoSelectionEvent [
	
	| counter |
	counter := 0.
	presenter
		selectIndex: 1;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectIndex: 2.
	self assert: counter equals: 0
]

{ #category : #'tests-unselect-item' }
ListPresenterMultipleSelectionTest >> testUnselectUnselectedItemKeepsSelectionList [

	presenter
		selectItem: 10;
		unselectItem: 20.
	self assert: presenter selection selectedItems asArray equals: #(10)
]

{ #category : #'tests-unselect-item' }
ListPresenterMultipleSelectionTest >> testUnselectUnselectedItemRaisesNoSelectionEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 20.
	self assert: counter equals: 0
]
