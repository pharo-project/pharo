actions
selectedAnyFileDirectory
	"If a directory is selected in the 'File' pane, return that. Otherwise, return the directory selected in 'Directory pane (i.e. the tree)"

	| selectedFile isDirectorySelected |
	selectedFile := self selectedFileEntry.
	isDirectorySelected := selectedFile isNil not and: [ selectedFile isDirectory ].
	^ isDirectorySelected
			ifTrue: [ selectedFile asFileReference ]
			ifFalse: [ self selectedFileDirectory ].