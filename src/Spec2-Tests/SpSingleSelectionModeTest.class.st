Class {
	#name : #SpSingleSelectionModeTest,
	#superclass : #SpAbstractSelectionModeTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #tests }
SpSingleSelectionModeTest >> testDropListSelectionIsNotAffectedBySorting [
	"To get the proper selection when a presenter is sorted, the selection asks the adapter to return the right element according to the sorting.
	DropList does not share the same hierarchy as other lists.
	We need to ensure that drop list is polymorphic with other lists and handle correctly the selection"
	presenter := SpDropListPresenter new.
	presenter 
		items: (1 to: 10) asArray;
		selectIndex: 3;
		openWithSpec.
		
	self assert: presenter selection selectedItem model equals: 3
]

{ #category : #tests }
SpSingleSelectionModeTest >> testDropListSelectionIsResetAfterItemsAssignment [
	presenter := SpDropListPresenter new.
	presenter
		items: (1 to: 10) asArray;
		selectIndex: 1.

	presenter items: #(2 5).

	self 
		assert: presenter selection selectedItem
		equals: nil
]

{ #category : #tests }
SpSingleSelectionModeTest >> testGetRightElementAfterSortingOfElementsChanged [
	presenter := SpTablePresenter new. "use a TablePresenter since ListPresenter cannot sort"
	presenter 
		items: (1 to: 10) asArray;
		beSingleSelection;
		openWithSpec.
	
	presenter 
		withAdapterDo: [ :adapter | adapter widget dataSource sortElements: [ :a :b | a > b ] ];
		selectIndex: 3.
	
	self assert: presenter selection selectedItem equals: 8
]

{ #category : #tests }
SpSingleSelectionModeTest >> testSelectionIsResetAfterItemsAssignment [
	presenter
		beSingleSelection;
		openWithSpec;
		selectIndex: 1.

	presenter items: #(2 5).

	self 
		assert: presenter selection selectedItem 
		equals: nil
]

{ #category : #tests }
SpSingleSelectionModeTest >> testSelectionIsResetAfterSorting [
	presenter := SpTablePresenter new.	"use a TablePresenter since ListPresenter cannot sort"
	presenter
		items: (1 to: 10) asArray;
		beSingleSelection;
		openWithSpec;
		selectIndex: 1.
		
	presenter
		withAdapterDo: [ :adapter | 
			adapter widget dataSource
				sortByColumn: presenter adapter columns first
				event: nil
				morph: nil ].
			
	self assert: presenter selection selectedItem equals: nil
]

{ #category : #tests }
SpSingleSelectionModeTest >> testSubscriptionsAreTransfered [
	| count |
	count := 0.
	presenter beSingleSelection.
	presenter whenSelectionChangedDo: [ count := count + 1 ].
	presenter beMultipleSelection.
	presenter selectIndex: 2.
	self assert: count equals: 1
]
