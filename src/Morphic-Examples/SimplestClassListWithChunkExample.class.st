"
SimplestClassListWithChunkExample new openOn: Object
"
Class {
	#name : 'SimplestClassListWithChunkExample',
	#superclass : 'SimplestPaginatedClassListExample',
	#category : 'Morphic-Examples-MorphTreeWidget',
	#package : 'Morphic-Examples',
	#tag : 'MorphTreeWidget'
}

{ #category : 'examples' }
SimplestClassListWithChunkExample class >> example [
	<sampleInstance>

	^SimplestClassListWithChunkExample new openOn: Object
]

{ #category : 'defaults' }
SimplestClassListWithChunkExample >> defaultTreeMorph [
	^ super defaultTreeMorph chunkSize: 100; yourself
]
