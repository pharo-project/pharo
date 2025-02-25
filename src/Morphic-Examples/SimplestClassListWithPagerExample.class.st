"
SimplestClassListWithPagerExample new openOn: Object
"
Class {
	#name : 'SimplestClassListWithPagerExample',
	#superclass : 'SimplestPaginatedClassListExample',
	#category : 'Morphic-Examples-MorphTreeWidget',
	#package : 'Morphic-Examples',
	#tag : 'MorphTreeWidget'
}

{ #category : 'examples' }
SimplestClassListWithPagerExample class >> example [
	<sampleInstance>

	^SimplestClassListWithPagerExample new openOn: Object
]

{ #category : 'defaults' }
SimplestClassListWithPagerExample >> defaultTreeMorph [
	^ super defaultTreeMorph pageSize: 100; yourself
]
