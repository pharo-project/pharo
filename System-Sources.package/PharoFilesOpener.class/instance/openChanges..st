open changes
openChanges: changesPath
	| fileColocatedWithImage fileInWorkingDirectory locations |
	fileColocatedWithImage := changesPath asFileReference.
	fileInWorkingDirectory := FileSystem workingDirectory / fileColocatedWithImage basename.	"locations references all possible path where to search for the .changes file."
	locations := Array with: fileColocatedWithImage with: fileInWorkingDirectory.	
		
	"We first try to open a writeStream and, if we can't, we open a readStream. We have to use #ignoreIfFail: here because Pharo has no way to really tell us beforehand if opening a writeStream will work. Sending #isWritable is not enough because #isWritable does not check if the current user is the owner of the file or not."
	locations do: [ :file | self ignoreIfFail: [ ^ file writeStream ] ].
	locations do: [ :file | self ignoreIfFail: [ ^ file readStream ] ].
	^ nil