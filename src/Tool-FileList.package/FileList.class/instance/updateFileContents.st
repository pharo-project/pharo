private
updateFileContents
	| choice |
	choice := self confirmOverwriteOrCreateNewFileOrAbort.
	
	choice = 1 ifTrue:[ self truncatedAndOverwriteFilecontents].
	choice = 2 ifTrue:[ self requestNewFileNameAndSave].
