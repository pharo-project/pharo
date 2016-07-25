accessing
renameChangesForClass: oldClassName to: newClassName
	| change |
	change := super renameChangesForClass: oldClassName to: newClassName.
	oldName asSymbol = oldClassName
		ifTrue: [ change rename: newClassName to: newName ].
	^ change