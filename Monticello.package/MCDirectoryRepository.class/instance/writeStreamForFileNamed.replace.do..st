i/o
writeStreamForFileNamed: aString replace: shouldReplace do: aBlock

	| file |
	file := directory / aString.

	shouldReplace ifTrue: [ file ensureDelete ].

	file writeStreamDo:  [ :stream | 
		aBlock value: stream ].