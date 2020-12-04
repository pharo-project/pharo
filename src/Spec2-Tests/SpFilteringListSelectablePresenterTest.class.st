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
SpFilteringListSelectablePresenterTest >> testSelectedItemsOrderedBasedOnList [

	listWithFilter items: { 
		OrderedCollection.
		Array.
		SequenceableCollection.
		Set.
		Dictionary }.

	listWithFilter selectedItems: { Set. Array }.
	
	self 
		assert: listWithFilter selectedItems
		equals: { Array. Set }
]
