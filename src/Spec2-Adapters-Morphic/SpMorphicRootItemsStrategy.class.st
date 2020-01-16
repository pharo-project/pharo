Class {
	#name : #SpMorphicRootItemsStrategy,
	#superclass : #FTRootItemsStrategy,
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #accessing }
SpMorphicRootItemsStrategy >> isMatching: anItem [

	^ dataSource model 
		performSearch: anItem data 
		matching: pattern
]
