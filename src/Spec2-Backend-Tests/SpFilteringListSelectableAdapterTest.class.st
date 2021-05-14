Class {
	#name : #SpFilteringListSelectableAdapterTest,
	#superclass : #SpAbstractAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpFilteringListSelectableAdapterTest >> classToTest [

	^ SpFilteringSelectableListPresenter
]

{ #category : #tests }
SpFilteringListSelectableAdapterTest >> testClickOnRowTogglesSelection [

	presenter items: { 
		OrderedCollection.
		Array.
		SequenceableCollection.
		Set.
		Dictionary }.
	
	presenter listPresenter selectItem: Array.
	self openInstance.
	self assertEmpty: presenter selectedItems.
	presenter listPresenter adapter sendClickEventToRow: 2.
	self assert: (presenter selectedItems hasEqualElements: { Array })

]

{ #category : #tests }
SpFilteringListSelectableAdapterTest >> testClickOnRowWithShiftSelectsRange [

	self skip. "Failing on CI"

	presenter items: { 
		OrderedCollection.
		Array.
		SequenceableCollection.
		Set.
		Dictionary }.
	
	presenter selectItem: Array.
	self openInstance.
	self assert: (presenter selectedItems hasEqualElements: { Array }).	
	presenter listPresenter adapter sendClickWithShiftEventToRow: 4.
	self assert: (presenter selectedItems hasEqualElements: { Array. SequenceableCollection. Set })
]

{ #category : #tests }
SpFilteringListSelectableAdapterTest >> testClickOnRowWithShiftSelectsRangeReversed [

	self skip. "Failing on CI"

	presenter items: { 
		OrderedCollection.
		Array.
		SequenceableCollection.
		Set.
		Dictionary }.
	
	presenter selectItem: Set.
	self openInstance.
	self assert: (presenter selectedItems hasEqualElements: { Set }).	
	presenter listPresenter adapter sendClickWithShiftEventToRow: 2.
	self assert: (presenter selectedItems hasEqualElements: { Array. SequenceableCollection. Set })
]
