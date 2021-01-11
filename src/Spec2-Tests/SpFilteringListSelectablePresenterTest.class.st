Class {
	#name : #SpFilteringListSelectablePresenterTest,
	#superclass : #SpFilteringListPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #accessing }
SpFilteringListSelectablePresenterTest >> classToTest [

	^ SpFilteringSelectableListPresenter
]

{ #category : #tests }
SpFilteringListSelectablePresenterTest >> testSelectItemAccumulatesSelections [

	listWithFilter items: { 
		OrderedCollection.
		Array.
		SequenceableCollection.
		Set.
		Dictionary }.

	listWithFilter selectItem: Set.
	listWithFilter selectItem: Array.
	
	self 
		assert: listWithFilter selectedItems
		equals: { Array. Set }
]

{ #category : #tests }
SpFilteringListSelectablePresenterTest >> testSelectedItemsOrderedBasedOnList [

	listWithFilter items: { 
		OrderedCollection.
		Array.
		SequenceableCollection.
		Set.
		Dictionary }.

	listWithFilter selectItems: { Set. Array }.
	
	self 
		assert: listWithFilter selectedItems
		equals: { Array. Set }
]
