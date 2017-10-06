public
move: sourcePath to: destination
	"Move the file /directory referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If destPath is a directory, move the sourcePath in to the directory"

	| fullDestination |

	destination isFile ifTrue: [ FileExists signalWith: destination ].
	destination isDirectory
		ifTrue: [ fullDestination := destination / sourcePath basename ]
		ifFalse: [ fullDestination := destination ].
	self = destination fileSystem ifTrue: 
	[
		"Ideally we would test whether the source and destination are on the same filesystem from the OSs perspective.
		Since we can't do that, just try rename, and if that fails, copy and delete."
		[ self rename: sourcePath to: fullDestination resolve path ]
			on: Error
			do: [ :error | self copyAndDelete: sourcePath to: fullDestination ].
	] ifFalse:
		[ self copyAndDelete: sourcePath to: fullDestination ].
	^fullDestination