testing
isNextChunkMetaData
	| isMetaData |
	isMetaData := decoratedStream peek = $!.
	isMetaData ifTrue: [ decoratedStream next ].
	^ isMetaData