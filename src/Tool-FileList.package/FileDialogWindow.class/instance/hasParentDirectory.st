testing
hasParentDirectory
	"Answer whether the selected directory in the tree part has a parent."

	^ self selectedFileDirectory ifNotNil: [ :dir | dir isRoot not ] ifNil: [ false ].