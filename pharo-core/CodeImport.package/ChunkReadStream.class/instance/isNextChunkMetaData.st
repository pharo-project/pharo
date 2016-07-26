testing
isNextChunkMetaData
	| isMetaData |
	isMetaData := decoratedStream peek = $!.
	isMetaData ifTrue: [ decoratedStream basicNext ].
	^ isMetaData