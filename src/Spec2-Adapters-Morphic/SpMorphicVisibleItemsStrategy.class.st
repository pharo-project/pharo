Class {
	#name : #SpMorphicVisibleItemsStrategy,
	#superclass : #FTVisibleItemsStrategy,
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #accessing }
SpMorphicVisibleItemsStrategy >> isMatching: anItem [

	^ dataSource model 
		performSearch: anItem data
		matching: pattern
]
