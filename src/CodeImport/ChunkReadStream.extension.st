Extension { #name : 'ChunkReadStream' }

{ #category : '*CodeImport' }
ChunkReadStream >> isNextChunkMetaData [
	| isMetaData |
	isMetaData := decoratedStream peek = $!.
	isMetaData ifTrue: [ decoratedStream next ].
	^ isMetaData
]
