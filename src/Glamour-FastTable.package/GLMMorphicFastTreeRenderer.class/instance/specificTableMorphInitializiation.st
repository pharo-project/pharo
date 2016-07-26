initialization
specificTableMorphInitializiation
	| presentation |
	super specificTableMorphInitializiation.
	presentation := tableModel glamourPresentation.
	"If needed I extant the tree"
	presentation shouldAllExpand
		ifTrue: [ tableModel expandAll ].
	presentation shouldRootsExpand
		ifTrue: [ tableModel expandRoots ].
	presentation shouldExpandToLevel
		ifTrue: [ tableModel expandAllTo: presentation expandLevel ]