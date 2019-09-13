Class {
	#name : #SpListPresenterMultipleSelectionTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpListPresenterMultipleSelectionTest >> classToTest [
	^ SpListPresenter
]

{ #category : #running }
SpListPresenterMultipleSelectionTest >> setUp [
	super setUp.
	presenter
		beMultipleSelection;
		items: #(10 20 30)
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectAllRaisesSelectionEventOnce [
	"Because it does nothing in single selection mode"
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter selectAll.

	self assert: events equals: 1
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectAllSelectsAllItems [

	presenter selectAll.
	self assert: presenter selection selectedItems asSet equals: presenter model items asSet
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexAddsIndexToSelectedIndexList [

	presenter selectIndex: 1.
	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexAddsItemToSelectedItemList [

	presenter selectIndex: 1.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexOutsideRangeHasNoSelectedIndexes [

	presenter selectIndex: 4.
	self assert: presenter selection selectedIndexes isEmpty
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexOutsideRangeHasNoSelectedItems [

	presenter selectIndex: 4.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexOutsideRangeIsEmpty [

	presenter selectIndex: 4.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexThenSelectOutsideRangeKeepsFirstElement [

	presenter selectIndex: 1.
	presenter selectIndex: 50.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexThenSelectOutsideRangeKeepsFirstIndex [

	presenter selectIndex: 1.
	presenter selectIndex: 50.
	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexThenSelectOutsideRangeKeepsSingleSelectedItem [

	presenter selectIndex: 1.
	presenter selectIndex: 50.
	self assert: presenter selection selectedItems size equals: 1
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexTwiceAddsIndexToSelectedIndexListOnlyOnce [

	presenter
		selectIndex: 1;
		selectIndex: 1.
	self assert: presenter selection selectedIndexes asArray equals: #(1)
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexesAddsIndexesToSelectedIndexList [
	presenter selectIndexes: {1 . 2}.
	self assert: (presenter selection includesIndexes: {1 . 2})
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexesAddsItemsToSelectedItemList [
	presenter selectIndexes: {1 . 2}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexesOutsideRangeHasNoSelectedIndexes [
	presenter selectIndexes: {4 . 5}.
	self assert: presenter selection selectedIndexes isEmpty
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexesOutsideRangeHasNoSelectedItems [
	presenter selectIndexes: {4 . 5}.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexesOutsideRangeIsEmpty [
	presenter selectIndexes: {4 . 5}.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexesThenSelectOutsideRangeKeepsElements [
	presenter selectIndexes: {1 . 2}.
	presenter selectIndexes: {50 . 60}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexesThenSelectOutsideRangeKeepsIndexes [
	presenter selectIndexes: {1 . 2}.
	presenter selectIndexes: {50 . 60}.
	self assert: (presenter selection includesIndexes: {1 . 2})
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectIndexesTwiceAddsIndexesToSelectedIndexListOnlyOnce [
	presenter
		selectIndexes: {1 . 2};
		selectIndexes: {1 . 2}.
	self assertCollection: presenter selection selectedIndexes hasSameElements: {1 . 2}
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemAddsIndexToSelectedIndexList [

	presenter selectItem: 10.
	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemAddsItemToSelectedItemList [

	presenter selectItem: 10.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemOutsideRangeHasNoSelectedIndexes [

	presenter selectItem: 400.
	self assert: presenter selection selectedIndexes isEmpty
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemOutsideRangeHasNoSelectedItems [

	presenter selectItem: 400.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemOutsideRangeIsEmpty [

	presenter selectItem: 4000.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsFirstElement [

	presenter selectItem: 10.
	presenter selectItem: 5000.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsFirstIndex [

	presenter selectItem: 10.
	presenter selectItem: 500.
	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsSingleSelectedItem [

	presenter selectItem: 10.
	presenter selectItem: 3000.
	self assert: presenter selection selectedItems size equals: 1
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemsAddsIndexesToSelectedIndexList [
	presenter selectItems: {10 . 20}.
	self assert: (presenter selection includesIndexes: #(1 2))
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemsAddsItemsToSelectedItemList [
	presenter selectItems: {10 . 20}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemsOutsideRangeHasNoSelectedIndexes [
	presenter selectItems: {300 . 400}.
	self assert: presenter selection selectedIndexes isEmpty
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemsOutsideRangeHasNoSelectedItems [
	presenter selectItems: {300 . 400}.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemsOutsideRangeIsEmpty [
	presenter selectItems: {4000 . 5000}.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemsThenSelectOutsideRangeKeepsElements [
	presenter selectItems: {10 . 20}.
	presenter selectItems: {4000 . 5000}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectItemsThenSelectOutsideRangeKeepsIndexes [
	presenter selectItems: {10 . 20}.
	presenter selectItems: {500 . 600}.
	self assert: (presenter selection includesIndexes: {1 . 2})
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectMultipleIndexesAddsAllToSelectedIndexList [

	presenter selectIndex: 1.
	presenter selectIndex: 3.
	self assert: (presenter selection includesIndex: 1).
	self assert: (presenter selection includesIndex: 3).
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectMultipleIndexesAddsAllToSelectedItemList [

	presenter selectIndex: 1.
	presenter selectIndex: 3.
	self assert: (presenter selection includesItem: 10).
	self assert: (presenter selection includesItem: 30).
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSelectMultipleIndexesRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter	selectIndex: 1.
	presenter	selectIndex: 3.

	self assert: events equals: 2
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectMultipleItemAddsAllToSelectedIndexList [

	presenter selectItem: 10.
	presenter selectItem: 30.
	self assert: (presenter selection includesIndex: 1).
	self assert: (presenter selection includesIndex: 3).
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectMultipleItemsAddsAllToSelectedItemList [

	presenter selectItem: 10.
	presenter selectItem: 30.
	self assert: (presenter selection includesItem: 10).
	self assert: (presenter selection includesItem: 30).
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSelectMultipleItemsRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter selectItem: 10.
	presenter selectItem: 30.

	self assert: events equals: 2
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSetSelectIndexOutsideRangeDoesNotModifySelection [

	presenter whenSelectionChangedDo: [ :selection | self fail ].
	
	presenter selectIndex: 4.
	"If we arrive here and the test did not fail, we succeeded"
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSetSelectIndexRaisesSelectionChangeEventWithSelectedIndex [
	| selectedIndexes |
	presenter
		whenSelectionChangedDo: [ :selection | selectedIndexes := selection selectedIndexes ].
	presenter selectIndex: 1.
	self assert: (selectedIndexes includes: 1)
]

{ #category : #'tests-select-index' }
SpListPresenterMultipleSelectionTest >> testSetSelectIndexRaisesSelectionChangeEventWithSelectedItem [
	| selectedItems |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItems := selection selectedItems ].
	presenter selectIndex: 1.
	self assert: (selectedItems includes: 10)
]

{ #category : #'tests-select-item' }
SpListPresenterMultipleSelectionTest >> testSetSelectItemOutsideRangeDoesNotModifySelection [

	presenter whenSelectionChangedDo: [ :selection | self fail ].
	
	presenter selectItem: 100.
	"If we arrive here and the test did not fail, we succeeded"
]

{ #category : #'tests-unselect-index' }
SpListPresenterMultipleSelectionTest >> testUnselectAllRaisesSelectionEventOnce [
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
SpListPresenterMultipleSelectionTest >> testUnselectAllUnselectsall [

	presenter
		selectIndex: 1;
		unselectAll.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-index' }
SpListPresenterMultipleSelectionTest >> testUnselectSelectedIndexRaisesSelectionEventOnce [
	
	| counter |
	counter := 0.
	presenter
		selectIndex: 1;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectIndex: 1.
	self assert: counter equals: 1
]

{ #category : #'tests-unselect-index' }
SpListPresenterMultipleSelectionTest >> testUnselectSelectedIndexRemovesItFromSelectionList [

	presenter
		selectIndex: 1;
		unselectIndex: 1.
	self assert: (presenter selection isEmpty)
]

{ #category : #'tests-unselect-item' }
SpListPresenterMultipleSelectionTest >> testUnselectSelectedItemRaisesSelectionEventOnce [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 10.
	self assert: counter equals: 1
]

{ #category : #'tests-unselect-item' }
SpListPresenterMultipleSelectionTest >> testUnselectSelectedItemRemovesItFromSelectionList [

	presenter
		selectItem: 10;
		unselectItem: 10.
	self assert: (presenter selection isEmpty)
]

{ #category : #'tests-unselect-index' }
SpListPresenterMultipleSelectionTest >> testUnselectUnselectedIndexKeepsSelectionList [

	presenter
		selectIndex: 1;
		unselectIndex: 2.
	self assert: presenter selection selectedIndexes asArray equals: #(1)
]

{ #category : #'tests-unselect-index' }
SpListPresenterMultipleSelectionTest >> testUnselectUnselectedIndexRaisesNoSelectionEvent [
	
	| counter |
	counter := 0.
	presenter
		selectIndex: 1;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectIndex: 2.
	self assert: counter equals: 0
]

{ #category : #'tests-unselect-item' }
SpListPresenterMultipleSelectionTest >> testUnselectUnselectedItemKeepsSelectionList [

	presenter
		selectItem: 10;
		unselectItem: 20.
	self assert: presenter selection selectedItems asArray equals: #(10)
]

{ #category : #'tests-unselect-item' }
SpListPresenterMultipleSelectionTest >> testUnselectUnselectedItemRaisesNoSelectionEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 20.
	self assert: counter equals: 0
]
