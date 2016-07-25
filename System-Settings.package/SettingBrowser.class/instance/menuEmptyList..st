menu
menuEmptyList: aTreeNode
	| v |
	(v := aTreeNode settingDeclaration) hasEditableList 
		ifTrue: [v emptyList. self updateList]