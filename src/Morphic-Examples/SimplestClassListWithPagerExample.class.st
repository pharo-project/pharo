"
SimplestClassListWithPagerExample new openOn: Object
"
Class {
	#name : #SimplestClassListWithPagerExample,
	#superclass : #SimplestPaginatedClassListExample,
	#category : #Morphic-Examples-MorphTreeWidget
}

{ #category : #examples }
SimplestClassListWithPagerExample class >> example [

	^SimplestClassListWithPagerExample new openOn: Object
]

{ #category : #defaults }
SimplestClassListWithPagerExample >> defaultTreeMorph [
	^ super defaultTreeMorph pageSize: 100; yourself

]
