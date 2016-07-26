public - string reading
getPreambleFrom: aFileStream at: position

	| writeStream |
	writeStream := String new writeStream.
	position to: 0 by: -1 do: [ :index | 
		| aCharacter |
		aFileStream position: index.
		aCharacter := aFileStream basicNext.
		aCharacter == $!
			ifTrue: [ ^ writeStream contents reversed ]
			ifFalse: [ writeStream nextPut: aCharacter ] ]