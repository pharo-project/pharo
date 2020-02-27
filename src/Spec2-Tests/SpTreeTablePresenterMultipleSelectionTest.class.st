Class {
	#name : #SpTreeTablePresenterMultipleSelectionTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpTreeTablePresenterMultipleSelectionTest >> classToTest [
	^ SpTreeTablePresenter
]

{ #category : #running }
SpTreeTablePresenterMultipleSelectionTest >> setUp [
	super setUp.
	presenter
		addColumn: (SpStringTableColumn title: 'Value' evaluated: #printString);
		beMultipleSelection;
		roots: #(1 2 3);
		children: [ :aNumber | 
			aNumber < 100
				ifTrue: [ { aNumber * 2. aNumber * 3. aNumber * 10 } ]
				ifFalse: [ #() ] ];
		yourself.
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectAbsentItemGivesEmptySelection [

	presenter selectItem: 4000.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectInvalidPathHasNoSelectedItems [

	presenter selectPath: #(4).
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectInvalidPathHasNoSelectedPaths [

	presenter selectPath: #(4).
	self assert: presenter selection selectedPaths isEmpty
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectInvalidPathsHasNoSelectedItems [
	presenter selectPaths: { #(10 20) . #(20 20) }.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectInvalidPathsHasNoSelectedPaths [
	presenter selectPaths: { #(10 20) . #(20 20) }.
	self assert: presenter selection selectedPaths isEmpty
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectInvalidPathsIsEmpty [
	presenter selectPaths: { #(40) . #(10 20)}.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemAddsItemToSelectedItemList [

	presenter selectItem: 10.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemAddsPathToSelectedPathList [

	presenter selectItem: 10.
	self assert: (presenter selection includesPath: #(1 3))
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemOutsideRangeHasNoSelectedItems [

	presenter selectItem: 4000.
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemOutsideRangeHasNoSelectedPath [

	presenter selectItem: 4000.
	self assert: presenter selection selectedPaths isEmpty
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsFirstElement [

	presenter selectItem: 10.
	presenter selectItem: 5000.
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsFirstPath [

	presenter selectItem: 10.
	presenter selectItem: 5000.
	self 
		assertCollection: presenter selection selectedPaths
		hasSameElements: #( #(1 3) )
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemThenSelectOutsideRangeKeepsSingleSelectedItem [

	presenter selectItem: 10.
	presenter selectItem: 3000.
	self assert: presenter selection selectedItems size equals: 1
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemsAddsItemsToSelectedItemList [
	presenter selectItems: {10 . 20}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemsAddsPathsToSelectedPathList [
	presenter selectItems: {10 . 20}.

	self 
		assertCollection: presenter selection selectedPaths 
		hasSameElements: { #(1 3) . #(1 1 3) }
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemsOutsideRangeHasNoSelectedItems [
	presenter selectItems: {3000 . 4000}.
	
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemsOutsideRangeHasNoSelectedPaths [
	presenter selectItems: {3000 . 4000}.
	
	self assert: presenter selection selectedPaths isEmpty
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemsOutsideRangeIsEmpty [
	presenter selectItems: {4000 . 5000}.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemsThenSelectOutsideRangeKeepsElements [
	presenter selectItems: {10 . 20}.
	presenter selectItems: {4000 . 5000}.
	self assert: (presenter selection includesItems: {10 . 20})
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectItemsThenSelectOutsideRangeKeepsPaths [
	presenter selectItems: {10 . 20}.
	presenter selectItems: {5000 . 6000}.
	
	self 
		assertCollection: presenter selection selectedPaths
		hasSameElements: { #(1 1 3) . #(1 3) }
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectMultipleItemAddsAllToSelectedPathList [

	presenter 
		selectItem: 10;
		selectItem: 30.
	self 
		assertCollection: presenter selection selectedPaths
		hasSameElements: { #(1 3) . #(1 2 3) }
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectMultipleItemsAddsAllToSelectedItemList [

	presenter selectItem: 10.
	presenter selectItem: 30.
	self assert: (presenter selection includesItem: 10).
	self assert: (presenter selection includesItem: 30).
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectMultipleItemsRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter selectItem: 10.
	presenter selectItem: 30.

	self assert: events equals: 2
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectMultiplePathsAddsAllToSelectedItemList [

	presenter 
		selectPath: #(1 2);
		selectPath: #(2 2).
	self assert: (presenter selection includesItem: 3).
	self assert: (presenter selection includesItem: 6).
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectMultiplePathsAddsAllToSelectedPathList [

	presenter 
		selectPath: #(1 2);
		selectPath: #(2 2).
	self assert: (presenter selection includesItem: 3).
	self assert: (presenter selection includesItem: 6).

	self assert: (presenter selection includesPath: #(1 2)).
	self assert: (presenter selection includesPath: #(2 2)).
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectMultiplePathsRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter	selectPath: #(1 1).
	presenter	selectPath: #(3).

	self assert: events equals: 2
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathAddsIndexToSelectedPathList [

	presenter selectPath: #(1).
	self assert: (presenter selection includesPath: #(1))
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathAddsItemToSelectedItemList [

	presenter selectPath: #(1 3).
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathRaisesSelectionChangeEventWithSelectedPath [
	| selectedPaths |
	presenter
		whenSelectionChangedDo: [ :selection | selectedPaths := selection selectedPaths ].
		
	presenter selectPath: #(1 2).
	
	self assert: (selectedPaths includes: #(1 2)).
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathThenSelectInvalidPathKeepsFirstElement [
	
	presenter selectPath: #(1 3).
	presenter selectPath: #(50).
	self assert: (presenter selection includesItem: 10)
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathThenSelectInvalidPathKeepsFirstPath [
	
	presenter selectPath: #(1 3).
	presenter selectPath: #(50).
	self assert: (presenter selection includesPath: #(1 3))
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathThenSelectInvalidPathKeepsSingleSelectedItem [
	
	presenter selectPath: #(1 3).
	presenter selectPath: #(50).
	self 
		assert: presenter selection selectedItems size 
		equals: 1
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathTwiceAddsPathToSelectedPathListOnlyOnce [

	presenter selectPath: #(1 3).
	presenter selectPath: #(1 3).
	self 
		assert: presenter selection selectedPaths
		equals: #( #(1 3) )
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathsAddsItemsToSelectedItemList [
	| paths |
	paths := { #(1 2) . #(2 2) }.
	presenter selectPaths: paths.
	
	self assert: (presenter selection includesItems: {3 . 6})
	
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathsAddsPathsToSelectedPathList [
	| paths |
	paths := { #(1 2) . #(2 2) }.
	presenter selectPaths: paths.
	self assert: (presenter selection includesPaths: paths).
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathsThenSelectInvalidPathKeepsElements [

	presenter 
		selectPaths: {#(1 3) . #(2 2)};
		selectPaths:	{#(50) .	#(60 2)}.
	self assert: (presenter selection includesItems: #(10 6))
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathsThenSelectInvalidPathKeepsPaths [
	| paths |
	paths := {#(1 3) . #(2 2)}.
	presenter 
		selectPaths: paths;
		selectPaths:	{#(50) .	#(60 2)}.
	self assert: (presenter selection includesPaths: paths)
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSelectPathsTwiceAddsPathssToSelectedPathListOnlyOnce [
	| paths |
	paths := {#(1 3) . #(2 2)}.
	presenter 
		selectPaths: paths;
		selectPaths:	paths.
	self 
		assertCollection: presenter selection selectedPaths 
		hasSameElements: paths
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSetSelectInvalidPathDoesNotModifySelection [

	presenter whenSelectionChangedDo: [ :selection | self fail ].
	
	presenter selectPath: #(50 1).
	"If we arrive here and the test did not fail, we succeeded".
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterMultipleSelectionTest >> testSetSelectItemOutsideRangeDoesNotModifySelection [

	presenter whenSelectionChangedDo: [ :selection | self fail ].
	
	presenter selectItem: 1000.
	"If we arrive here and the test did not fail, we succeeded"
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterMultipleSelectionTest >> testSetSelectPathRaisesSelectionChangeEventWithSelectedItem [
	| selectedItems |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItems := selection selectedItems ].
		
	presenter selectPath: #(1 3).
	
	self assert: (selectedItems includes: 10)
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectAllRaisesSelectionEventOnce [
	"Because it does nothing in single selection mode"

	| nbEvents |
	nbEvents := 0.

	"Be sure there is one element selected at least to make selection change"
	presenter selectPath: #(1 1).
	presenter
		whenSelectionChangedDo: [ :selection | nbEvents := nbEvents + 1 ].
		
	presenter unselectAll.
	
	self assert: nbEvents equals: 1
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectAllUnselectsall [

	presenter
		selectPath: #(1 2);
		unselectAll.
		
	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-item' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectSelectedItemRaisesSelectionEventOnce [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 10.
	self assert: counter equals: 1
]

{ #category : #'tests-unselect-item' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectSelectedItemRemovesItFromSelectionList [

	presenter
		selectItem: 10;
		unselectItem: 10.
	self assert: (presenter selection isEmpty)
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectSelectedPathRaisesSelectionEventOnce [
	
	| counter |
	counter := 0.
	presenter
		selectPath: #(1 2);
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectPath: #(1 2).
		
	self assert: counter equals: 1
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectSelectedPathRemovesItFromSelectionList [

	presenter
		selectPath: #(1 2);
		unselectPath: #(1 2).
	self assert: (presenter selection isEmpty)
]

{ #category : #'tests-unselect-item' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectUnselectedItemKeepsSelectionList [

	presenter
		selectItem: 10;
		unselectItem: 20.
	self assert: presenter selection selectedItems asArray equals: #(10)
]

{ #category : #'tests-unselect-item' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectUnselectedItemRaisesNoSelectionEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 20.
	self assert: counter equals: 0
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectUnselectedPathKeepsSelectionList [

	presenter
		selectPath: #(1 2);
		unselectPath: #(2 2).
	self 
		assert: presenter selection selectedPaths
		equals: { #(1 2) }
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterMultipleSelectionTest >> testUnselectUnselectedPathRaisesNoSelectionEvent [
	
	| counter |
	counter := 0.
	presenter
		selectPath: #(1 2);
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectPath: #(2 2).

	self assert: counter equals: 0
]
