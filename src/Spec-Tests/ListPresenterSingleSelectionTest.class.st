Class {
	#name : #ListPresenterSingleSelectionTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
ListPresenterSingleSelectionTest >> classToTest [
	^ ListPresenter
]

{ #category : #running }
ListPresenterSingleSelectionTest >> setUp [

	super setUp.

	window := presenter
		beSingleSelection;
		items: #(10 20 30);
		openWithSpec.
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectAllDoesNotRaiseEvent [
	"Because it does nothing in single selection mode"
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter selectAll.

	self assert: events equals: 0
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectAllDoesNotSelect [
	presenter selectAll.

	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectAllWithExistingSelectionLeavesSelection [
	presenter
		selectIndex: 1;
		selectAll.

	self assert: (presenter selection includesIndex: 1)
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectIndexOutsideRangeUnsetsSelectedIndex [
	presenter selectIndex: 4.

	self assert: (presenter selection includesIndex: 0)
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectIndexOutsideRangeUnsetsSelectedItem [

	presenter selectIndex: 4.
	self assert: presenter selection selectedItem equals: nil
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectIndexSetsSelectedIndex [

	presenter selectIndex: 1.
	self assert: presenter selection selectedIndex equals: 1
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectIndexSetsSelectedItem [

	presenter selectIndex: 1.
	self assert: presenter selection selectedItem equals: 10
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectIndexTwiceMakesIndexSelected [
	presenter
		selectIndex: 3;
		selectIndex: 3.

	self assert: (presenter selection includesIndex: 3)
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectIndexTwiceMakesIsListedOnceInSelectedIndexes [
	presenter
		selectIndex: 3;
		selectIndex: 3.

	self assert: presenter selection selectedIndexes asArray equals: #(3)
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSelectItemOutsideRangeUnsetsSelectedIndex [
	
	presenter selectItem: 40.
	self assert: presenter selection selectedIndex equals: 0
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSelectItemOutsideRangeUnsetsSelectedItem [

	presenter selectItem: 40.
	self assert: presenter selection selectedItem equals: nil
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSelectItemSetsSelectedIndex [

	presenter selectItem: 20.
	self assert: presenter selection selectedIndex equals: 2
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSelectItemSetsSelectedItem [
	presenter selectItem: 20.
	self assert: presenter selection selectedItem equals: 20
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSelectMultipleIndexesRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter	selectIndex: 1.
	presenter	selectIndex: 3.

	self assert: events equals: 2
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSetSelectIndexOutsideRangeRaisesSelectionChangeEventWithUnsetIndex [
	| selectedIndex |
	presenter
		whenSelectionChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectIndex: 4.
	self assert: selectedIndex equals: 0
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSetSelectIndexOutsideRangeRaisesSelectionChangeEventWithUnsetItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectIndex: 4.
	self assert: selectedItem equals: nil
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSetSelectIndexRaisesSelectionChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter
		whenSelectionChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectIndex: 1.
	self assert: selectedIndex equals: 1
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSetSelectIndexRaisesSelectionChangeEventWithSelectedItem [
	| selectedElement |

	presenter
		whenSelectionChangedDo: [ :selection | selectedElement := selection selectedItem ].
	presenter selectIndex: 1.
	self assert: selectedElement equals: 10
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSetSelectIndexRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | 
			selectedIndex := selection selectedIndex ].
	presenter selectIndex: 1.
	self assert: selectedIndex equals: 1
]

{ #category : #'tests-select-index' }
ListPresenterSingleSelectionTest >> testSetSelectIndexRaisesSelectionItemChangeEventWithSelectedIndex [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectIndex: 1.
	self assert: selectedItem equals: 10
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSetSelectItemOutsideRangeRaisesSelectionChangeEventWithUnsetIndex [
	| selectedIndex |

	presenter
		whenSelectionChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectItem: 40.
	self assert: selectedIndex equals: 0
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSetSelectItemOutsideRangeRaisesSelectionChangeEventWithUnsetItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectItem: 40.
	self assert: selectedItem equals: nil
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionChangeEventWithSelectedIndex [
	| selectedIndex |

	presenter
		whenSelectionChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectItem: 20.
	self assert: selectedIndex equals: 2
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionChangeEventWithSelectedItem [
	| selectedElement |
	presenter
		whenSelectionChangedDo: [ :selection | selectedElement := selection selectedItem ].
	presenter selectItem: 20.
	self assert: selectedElement equals: 20
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionIndexChangeEventWithSelectedIndex [
	| selectedIndex |
	presenter selection
		whenChangedDo: [ :selection | selectedIndex := selection selectedIndex ].
	presenter selectItem: 10.
	self assert: selectedIndex equals: 1
]

{ #category : #'tests-select-item' }
ListPresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionItemChangeEventWithSelectedItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectItem: 10.
	self assert: selectedItem equals: 10
]

{ #category : #'tests-unselect-index' }
ListPresenterSingleSelectionTest >> testUnselectAllRaisesSelectionEventOnce [
	"Because it does nothing in single selection mode"
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter unselectAll.

	self assert: events equals: 1
]

{ #category : #'tests-unselect-index' }
ListPresenterSingleSelectionTest >> testUnselectAllUnselectsSingleSelection [

	presenter
		selectIndex: 1;
		unselectAll.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-index' }
ListPresenterSingleSelectionTest >> testUnselectNonSelectedIndexDoesNotRemovesSelection [
	presenter
		selectIndex: 1;
		unselectIndex: 3.

	self assert: presenter selection selectedIndex equals: 1
]

{ #category : #'tests-unselect-index' }
ListPresenterSingleSelectionTest >> testUnselectNonSelectedIndexRaisesNoEvent [
	
	| counter |
	counter := 0.
	presenter
		selectIndex: 1;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectIndex: 2.

	self assert: counter equals: 0
]

{ #category : #'tests-unselect-item' }
ListPresenterSingleSelectionTest >> testUnselectNonSelectedItemDoesNotRemovesSelection [
	presenter
		selectItem: 10;
		unselectItem: 30.

	self assert: presenter selection selectedItem equals: 10
]

{ #category : #'tests-unselect-item' }
ListPresenterSingleSelectionTest >> testUnselectNonSelectedItemRaisesNoEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 20.

	self assert: counter equals: 0
]

{ #category : #'tests-unselect-index' }
ListPresenterSingleSelectionTest >> testUnselectSelectedIndexRaisesSingleEvent [
	
	| counter |
	counter := 0.
	presenter
		selectIndex: 1;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectIndex: 1.

	self assert: counter equals: 1
]

{ #category : #'tests-unselect-index' }
ListPresenterSingleSelectionTest >> testUnselectSelectedIndexRemovesSelection [
	presenter
		selectIndex: 1;
		unselectIndex: 1.

	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-item' }
ListPresenterSingleSelectionTest >> testUnselectSelectedItemRaisesSingleEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 10.

	self assert: counter equals: 1
]

{ #category : #'tests-unselect-item' }
ListPresenterSingleSelectionTest >> testUnselectSelectedItemRemovesSelection [

	presenter
		selectIndex: 10;
		unselectIndex: 10.

	self assert: presenter selection isEmpty
]
