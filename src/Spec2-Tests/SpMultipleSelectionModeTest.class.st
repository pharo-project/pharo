Class {
	#name : #SpMultipleSelectionModeTest,
	#superclass : #SpAbstractSelectionModeTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #tests }
SpMultipleSelectionModeTest >> testGetRightElementAfterSortingOfElementsChanged [
	presenter := SpTablePresenter new. "use a TablePresenter since ListPresenter cannot sort"
	presenter 
		items: (1 to: 10) asArray;
		beMultipleSelection;
		openWithSpec.
	
	presenter 
		withAdapterDo: [ :adapter | adapter widget dataSource sortElements: [ :a :b | a > b ] ];
		selectIndexes: #(1 3).
	
	self 
		assert: presenter selection selectedItems asArray 
		equals: #(10 8)
]

{ #category : #tests }
SpMultipleSelectionModeTest >> testSelectionIsResetAfterItemsAssignment [
	presenter
		beMultipleSelection;
		openWithSpec;
		selectIndexes: #(1 2 3).

	presenter items: #(2 5).

	self 
		assert: presenter selection selectedItems asArray
		equals: #()
]

{ #category : #tests }
SpMultipleSelectionModeTest >> testSelectionIsResetAfterSorting [
	presenter := SpTablePresenter new.	"use a TablePresenter since ListPresenter cannot sort"
	presenter
		items: (1 to: 10) asArray;
		beMultipleSelection;
		openWithSpec;
		selectIndexes: #(1 3).
		
	presenter
		withAdapterDo: [ :adapter | 
			adapter widget dataSource
				sortByColumn: presenter adapter columns first
				event: nil
				morph: nil ].
			
	self assert: presenter selection selectedItems isEmpty
]

{ #category : #tests }
SpMultipleSelectionModeTest >> testSubscriptionsAreTransfered [
	| count |
	count := 0.
	presenter beMultipleSelection.
	presenter whenSelectionChangedDo: [ count := count + 1 ].
	presenter beSingleSelection.
	presenter selectIndex: 2.
	self assert: count equals: 1
]
