enumerating
files
	"Answer the contents of the selected directory."

	self selectedFileDirectory ifNil: [ ^#() ].
	
	^Cursor wait showWhile: [ | cache filteredCache |
		cache := self cache: self selectedFileDirectory.
		filteredCache := cache select: self fileSelectionBlock.
		filteredCache asSortedCollection: self fileSortBlock ].