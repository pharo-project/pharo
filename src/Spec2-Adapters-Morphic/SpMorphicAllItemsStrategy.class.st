Class {
	#name : #SpMorphicAllItemsStrategy,
	#superclass : #FTAllItemsStrategy,
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #accessing }
SpMorphicAllItemsStrategy >> isMatching: anItem [

	^ dataSource model 
		performSearch: anItem data 
		matching: pattern
]
