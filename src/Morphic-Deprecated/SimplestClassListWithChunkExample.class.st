"
SimplestClassListWithChunkExample new openOn: Object
"
Class {
	#name : 'SimplestClassListWithChunkExample',
	#superclass : 'SimplestPaginatedClassListExample',
	#category : 'Morphic-Deprecated',
	#package : 'Morphic-Deprecated'
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
