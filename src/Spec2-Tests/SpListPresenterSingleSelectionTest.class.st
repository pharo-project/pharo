Class {
	#name : #SpListPresenterSingleSelectionTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpListPresenterSingleSelectionTest >> classToTest [
	^ SpListPresenter
]

{ #category : #running }
SpListPresenterSingleSelectionTest >> setUp [
	super setUp.

	presenter
		beSingleSelection;
		items: #(10 20 30)
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectAllDoesNotRaiseEvent [
	"Because it does nothing in single selection mode"
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter selectAll.

	self assert: events equals: 0
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectAllDoesNotSelect [
	presenter selectAll.

	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectAllWithExistingSelectionLeavesSelection [
	presenter
		selectIndex: 1;
		selectAll.

	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectIndexOutsideRangeUnsetsSelectedIndex [
	presenter selectIndex: 4.

	self assert: (presenter selection includesIndex: 0)
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectIndexOutsideRangeUnsetsSelectedItem [

	presenter selectIndex: 4.
	self assert: presenter selection selectedItem equals: nil
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectIndexSetsSelectedIndex [

	presenter selectIndex: 1.
	self assert: presenter selection selectedIndex equals: 1
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectIndexSetsSelectedItem [

	presenter selectIndex: 1.
	self assert: presenter selection selectedItem equals: 10
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectIndexTwiceMakesIndexSelected [
	presenter
		selectIndex: 3;
		selectIndex: 3.

	self assert: (presenter selection includesIndex: 3)
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectIndexTwiceMakesIsListedOnceInSelectedIndexes [
	presenter
		selectIndex: 3;
		selectIndex: 3.

	self assert: presenter selection selectedIndexes asArray equals: #(3)
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSelectItemOutsideRangeUnsetsSelectedIndex [
	
	presenter selectItem: 40.
	self assert: presenter selection selectedIndex equals: 0
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSelectItemOutsideRangeUnsetsSelectedItem [

	presenter selectItem: 40.
	self assert: presenter selection selectedItem equals: nil
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSelectItemSetsSelectedIndex [

	presenter selectItem: 20.
	self assert: presenter selection selectedIndex equals: 2
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSelectItemSetsSelectedItem [
	presenter selectItem: 20.
	self assert: presenter selection selectedItem equals: 20
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSelectMultipleIndexesRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter	selectIndex: 1.
	presenter	selectIndex: 3.

	self assert: events equals: 2
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSetSelectIndexOutsideRangeRaisesSelectionChangeEventWithUnsetIndex [
	| selectedIndex |
	presenter
		whenSelectionChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectIndex: 4.
	self assert: selectedIndex equals: 0
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSetSelectIndexOutsideRangeRaisesSelectionChangeEventWithUnsetItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectIndex: 4.
	self assert: selectedItem equals: nil
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSetSelectIndexRaisesSelectionChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter
		whenSelectionChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectIndex: 1.
	self assert: selectedIndex equals: 1
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSetSelectIndexRaisesSelectionChangeEventWithSelectedItem [
	| selectedElement |

	presenter
		whenSelectionChangedDo: [ :selection | selectedElement := selection selectedItem ].
	presenter selectIndex: 1.
	self assert: selectedElement equals: 10
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSetSelectIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | 
			selectedIndex := selection selectedIndex ].
	presenter selectIndex: 1.
	self assert: selectedIndex equals: 1
]

{ #category : #'tests-select-index' }
SpListPresenterSingleSelectionTest >> testSetSelectIndexRaisesSelectionItemChangeEventWithSelectedIndex [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectIndex: 1.
	self assert: selectedItem equals: 10
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSetSelectItemOutsideRangeRaisesSelectionChangeEventWithUnsetIndex [
	| selectedIndex |

	presenter
		whenSelectionChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectItem: 40.
	self assert: selectedIndex equals: 0
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSetSelectItemOutsideRangeRaisesSelectionChangeEventWithUnsetItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectItem: 40.
	self assert: selectedItem equals: nil
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionChangeEventWithSelectedIndex [
	| selectedIndex |

	presenter
		whenSelectionChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectItem: 20.
	self assert: selectedIndex equals: 2
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionChangeEventWithSelectedItem [
	| selectedElement |
	presenter
		whenSelectionChangedDo: [ :selection | selectedElement := selection selectedItem ].
	presenter selectItem: 20.
	self assert: selectedElement equals: 20
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectItem: 10.
	self assert: selectedIndex equals: 1
]

{ #category : #'tests-select-item' }
SpListPresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionItemChangeEventWithSelectedItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectItem: 10.
	self assert: selectedItem equals: 10
]

{ #category : #'tests-unselect-index' }
SpListPresenterSingleSelectionTest >> testUnselectAllRaisesSelectionEventOnce [
	"Because it does nothing in single selection mode"
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter unselectAll.

	self assert: events equals: 1
]

{ #category : #'tests-unselect-index' }
SpListPresenterSingleSelectionTest >> testUnselectAllUnselectsSingleSelection [

	presenter
		selectIndex: 1;
		unselectAll.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-index' }
SpListPresenterSingleSelectionTest >> testUnselectNonSelectedIndexDoesNotRemovesSelection [
	presenter
		selectIndex: 1;
		unselectIndex: 3.

	self assert: presenter selection selectedIndex equals: 1
]

{ #category : #'tests-unselect-index' }
SpListPresenterSingleSelectionTest >> testUnselectNonSelectedIndexRaisesNoEvent [
	
	| counter |
	counter := 0.
	presenter
		selectIndex: 1;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectIndex: 2.

	self assert: counter equals: 0
]

{ #category : #'tests-unselect-item' }
SpListPresenterSingleSelectionTest >> testUnselectNonSelectedItemDoesNotRemovesSelection [
	presenter
		selectItem: 10;
		unselectItem: 30.

	self assert: presenter selection selectedItem equals: 10
]

{ #category : #'tests-unselect-item' }
SpListPresenterSingleSelectionTest >> testUnselectNonSelectedItemRaisesNoEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 20.

	self assert: counter equals: 0
]

{ #category : #'tests-unselect-index' }
SpListPresenterSingleSelectionTest >> testUnselectSelectedIndexRaisesSingleEvent [
	
	| counter |
	counter := 0.
	presenter
		selectIndex: 1;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectIndex: 1.

	self assert: counter equals: 1
]

{ #category : #'tests-unselect-index' }
SpListPresenterSingleSelectionTest >> testUnselectSelectedIndexRemovesSelection [
	presenter
		selectIndex: 1;
		unselectIndex: 1.

	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-item' }
SpListPresenterSingleSelectionTest >> testUnselectSelectedItemRaisesSingleEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 10.

	self assert: counter equals: 1
]

{ #category : #'tests-unselect-item' }
SpListPresenterSingleSelectionTest >> testUnselectSelectedItemRemovesSelection [

	presenter
		selectItem: 10;
		unselectItem: 10.

	self assert: presenter selection isEmpty
]
