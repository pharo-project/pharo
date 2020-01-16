Class {
	#name : #SpTreeTablePresenterSingleSelectionTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpTreeTablePresenterSingleSelectionTest >> classToTest [
	^ SpTreeTablePresenter
]

{ #category : #running }
SpTreeTablePresenterSingleSelectionTest >> setUp [
	super setUp.

	super setUp.
	presenter
		addColumn: (SpStringTableColumn title: 'Value' evaluated: #printString);
		beSingleSelection;
		roots: #(1 2 3);
		children: [ :aNumber | 
			aNumber < 100
				ifTrue: [ { aNumber * 2. aNumber * 3. aNumber * 10 } ]
				ifFalse: [ #() ] ];
		yourself.
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSelectItemOutsideRangeUnsetsSelectedItem [

	presenter selectItem: 4000.
	self assert: presenter selection selectedItem equals: nil
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSelectItemOutsideRangeUnsetsSelectedPath [
	
	presenter selectItem: 4000.
	
	self 
		assert: presenter selection selectedPath equals: #()
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSelectItemSetsSelectedItem [
	presenter selectItem: 20.
	self assert: presenter selection selectedItem equals: 20
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSelectItemSetsSelectedPath [

	presenter selectItem: 20.
	self assert: presenter selection selectedPath equals: #(1 1 3)
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSelectMultiplePathsRaisesSelectionChangeEventMultipleTimes [
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter	selectPath: #(1).
	presenter	selectPath: #(1 3).

	self assert: events equals: 2
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSelectPathOutsideRangeUnsetsSelectedItem [
	presenter selectPath: { 4 }.
	
	self 
		assert: presenter selection selectedItem 
		equals: nil
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSelectPathOutsideRangeUnsetsSelectedPath [
	presenter selectPath: #(4).

	self 
		assert: presenter selection selectedPath
	 	equals: #()
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSelectPathSetsSelectedItem [

	presenter selectPath: #(1 3).
	
	self 
		assert: presenter selection selectedItem 
		equals: 10
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSelectPathSetsSelectedPath [

	presenter selectPath: #(1 1).
	
	self 
		assert: presenter selection selectedPath 
		equals: #(1 1)
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectItemOutsideRangeRaisesSelectionChangeEventWithUnsetItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectItem: 4000.
	self assert: selectedItem equals: nil
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectItemOutsideRangeRaisesSelectionChangeEventWithUnsetPath [
	| selectedPath |

	presenter
		whenSelectionChangedDo: [ :selection | selectedPath := selection selectedPath ].
	presenter selectItem: 4000.
	self assert: selectedPath equals: #()
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionChangeEventWithSelectedItem [
	| selectedElement |
	presenter
		whenSelectionChangedDo: [ :selection | selectedElement := selection selectedItem ].
	presenter selectItem: 20.
	self assert: selectedElement equals: 20
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionChangeEventWithSelectedPath [
	| selectedPath |

	presenter
		whenSelectionChangedDo: [ :selection | selectedPath := selection selectedPath ].
	presenter selectItem: 20.
	
	self assert: selectedPath equals: #(1 1 3)
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionItemChangeEventWithSelectedItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectItem: 10.
	self assert: selectedItem equals: 10
]

{ #category : #'tests-select-item' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectItemRaisesSelectionPathChangeEventWithSelectedPath [
	| selectedPath |
	
	presenter selection
		whenChangedDo: [ :selection | selectedPath := selection ].
	presenter selectItem: 10.
	
	self assert: selectedPath equals: #(1 3)
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectPathOutsideRangeRaisesSelectionChangeEventWithUnsetItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectPath: #(4).
	
	self assert: selectedItem equals: nil
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectPathOutsideRangeRaisesSelectionChangeEventWithUnsetPath [
	| selectedPath |
	presenter
		whenSelectionChangedDo: [ :selection | selectedPath := selection selectedPath ].
	presenter selectPath: #(4).
	
	self assert: selectedPath equals: nil
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectPathRaisesSelectionChangeEventWithSelectedItem [
	| selectedElement |

	presenter
		whenSelectionChangedDo: [ :selection | selectedElement := selection selectedItem ].
	presenter selectPath: #(1 3).
	
	self assert: selectedElement equals: 10
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectPathRaisesSelectionChangeEventWithSelectedPath [
	| selectedPath |
	presenter
		whenSelectionChangedDo: [ :selection | selectedPath := selection selectedPath ].
	presenter selectPath: #(1 2).
	
	self assert: selectedPath equals: #(1 2).
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectPathRaisesSelectionItemChangeEventWithSelectedItem [
	| selectedItem |
	presenter
		whenSelectionChangedDo: [ :selection | selectedItem := selection selectedItem ].
	presenter selectPath: #(1 3).
	
	self 
		assert: selectedItem 
		equals: 10
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testSetSelectPathRaisesSelectionPathChangeEventWithSelectedPath [
	| selectedPath |
	presenter selection
		whenChangedDo: [ :selection | selectedPath := selection ].
	presenter selectPath: #(1 2).
	
	self assert: selectedPath equals: #(1 2)
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectAllRaisesSelectionEventOnce [
	"Because it does nothing in single selection mode"
	| events |
	events := 0.
	presenter whenSelectionChangedDo: [ :selection | events := events + 1 ].

	presenter unselectAll.

	self assert: events equals: 1
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectAllUnselectsSingleSelection [

	presenter
		selectPath: #(1 1);
		unselectAll.
	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-item' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectNonSelectedItemDoesNotRemovesSelection [
	presenter
		selectItem: 10;
		unselectItem: 30.

	self 
		assert: presenter selection selectedItem 
		equals: 10
]

{ #category : #'tests-unselect-item' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectNonSelectedItemRaisesNoEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 20.

	self assert: counter equals: 0
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectNonSelectedPathDoesNotRemovesSelection [
	presenter
		selectPath: #(1 1);
		unselectPath: #(1 3).

	self 
		assert: presenter selection selectedPath
		equals: #(1 1)
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectNonSelectedPathRaisesNoEvent [
	
	| counter |
	counter := 0.
	presenter
		selectPath: #(1 1);
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectPath: #(2 1).

	self assert: counter equals: 0
]

{ #category : #'tests-unselect-item' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectSelectedItemRaisesSingleEvent [
	
	| counter |
	counter := 0.
	presenter
		selectItem: 10;
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectItem: 10.

	self assert: counter equals: 1
]

{ #category : #'tests-unselect-item' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectSelectedItemRemovesSelection [

	presenter
		selectItem: 10;
		unselectItem: 10.

	self assert: presenter selection isEmpty
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectSelectedPathRaisesSingleEvent [
	
	| counter |
	counter := 0.
	presenter
		selectPath: #(1 1);
		whenSelectionChangedDo: [ counter := counter + 1 ];
		unselectPath: #(1 1).

	self assert: counter equals: 1
]

{ #category : #'tests-unselect-index' }
SpTreeTablePresenterSingleSelectionTest >> testUnselectSelectedPathRemovesSelection [
	presenter
		selectPath: #(1 1);
		unselectPath: #(1 1).

	self assert: presenter selection isEmpty
]

{ #category : #'tests-select-index' }
SpTreeTablePresenterSingleSelectionTest >> testWhenSelectPathTwiceThenIsListedOnceInSelectedPaths [
	presenter
		selectPath: #(3 1);
		selectPath: #(3 1).

	self 
		assertCollection: presenter selection selectedPaths
		hasSameElements: { #(3 1) }
]
