menu
menuSetToDefault: aTreeNode
	| v |
	(v := aTreeNode settingDeclaration) hasDefault
		ifTrue: [(self confirm: 'Set to default value ?' translated)
				ifTrue: [v setToDefault. self updateList]]