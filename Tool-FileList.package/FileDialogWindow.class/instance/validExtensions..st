actions
validExtensions: aList
	"Set the filter for the files to be those with the given extensions."
	
	aList notEmpty
		ifTrue: [self defaultExtension: aList first].
	self fileSelectionBlock: [:de |
		de isDirectory
			ifTrue: [self showDirectoriesInFileList]
			ifFalse: [
				(self fileNamePattern match: de name) and: [
						aList includes: de extension asLowercase]]] 