public - string writing
changesWriteStreamDo: aBlock

	| changesFile |
	changesFile := self changesFileStream.
	changesFile ifNil: [ ^ self ].
	changesFile isReadOnly ifTrue: [ ^ self ].
	changesFile setToEnd.
	
	^ aBlock value: changesFile