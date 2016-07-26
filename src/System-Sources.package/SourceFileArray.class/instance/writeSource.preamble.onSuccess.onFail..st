public - string writing
writeSource: aString preamble: preamble onSuccess: successBlock onFail: failBlock

	| file position |
	file := self changesFileStream.
	(file isNil or: [ file isReadOnly ]) ifTrue: [ ^ failBlock value ].

	ChangesLog default assureStartupStampLogged.
	
	file setToEnd.
	file nextPutAll: preamble.
	position := file position.
	file nextChunkPut: aString.

	InMidstOfFileinNotification signal ifFalse: [ file flush ].

	successBlock value: (self sourcePointerFromFileIndex: 2 andPosition: position)