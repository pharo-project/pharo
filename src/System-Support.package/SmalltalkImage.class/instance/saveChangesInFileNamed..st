saving
saveChangesInFileNamed: aString
	| changesFile |
	changesFile := aString asFileReference.
	changesFile parent ensureCreateDirectory.
	"Ensure that the destination file is not the same as the original one"
	(Smalltalk changesFile absolutePath = changesFile absolutePath)
		ifFalse: [ 
			changesFile deleteIfAbsent: [  ].
			Smalltalk changesFile copyTo: changesFile ].