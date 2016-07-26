drag'n'drop
acceptDroppingMorph: aTransferMorph event: evt inMorph: dest
	| oldEntry destDirectory newEntry response |
	destDirectory := self dropDestinationDirectory: dest event: evt.
	oldEntry := FileSystem disk referenceTo: aTransferMorph passenger.
	newEntry := destDirectory / oldEntry basename.
	oldEntry = newEntry ifTrue: [ "Transcript nextPutAll: 'same as old name'; cr." ^ true ].
	newEntry ifNotNil: [ | msg |
		msg := String streamContents: [ :s |
			s nextPutAll: 'destination file ';
				nextPutAll: newEntry parent fullName;
				nextPutAll: ' exists already,';
				cr;
				nextPutAll: 'and is ';
				nextPutAll: (oldEntry modificationTime < newEntry modificationTime
					ifTrue: [ 'newer' ] ifFalse: [ 'not newer' ]);
				nextPutAll: ' than source file ';
				nextPutAll: oldEntry parent fullName;
				nextPut: $.;
				cr;
				nextPutAll: 'Overwrite file ';
				nextPutAll: newEntry basename;
				nextPut: $?
		].
		response := self confirm: msg.
		response ifFalse: [ ^false ].
	].

	aTransferMorph shouldCopy
		ifTrue: [ oldEntry copyAs: newEntry ]
		ifFalse: [ oldEntry renameTo: newEntry ].

	self updateFileList; fileListIndex: 0.

	aTransferMorph source model ~= self
		ifTrue: [ aTransferMorph source model updateFileList; fileListIndex: 0 ].
	"Transcript nextPutAll: 'copied'; cr."
	^true