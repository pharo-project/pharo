"
LazyClassListExample new openOn: Object

"
Class {
	#name : #LazyClassListExample,
	#superclass : #ClassListExample,
	#category : 'Morphic-Examples-MorphTreeWidget'
}

{ #category : #examples }
LazyClassListExample class >> example [

	^LazyClassListExample new openOn: Object

]

{ #category : #accessing }
LazyClassListExample >> treeMorphClass [
	^ LazyMorphTreeMorph 
]
