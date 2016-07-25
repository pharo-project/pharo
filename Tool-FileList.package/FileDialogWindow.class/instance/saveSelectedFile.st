actions
saveSelectedFile
	"Open a stream on the selected file if available and return it."

	|d f|
	
	d := self selectedFileDirectory ifNil: [^nil].
	f := self selectedFileName ifNil: [self fileNameText trimBoth].
	f ifEmpty: [ ^ nil ].
	
	f := d asFileReference / f.
	(f extension isEmpty and: [self defaultExtension notNil])
		 ifTrue: [ f := f, self defaultExtension].
	f exists 
		ifFalse: [ ^ f ].
		
	^(self
		proceed: ('The file {1} already exists.
Overwrite the file?' translated format: {f printString})
		title: 'Save File' translated)
			ifTrue: [ f delete ]