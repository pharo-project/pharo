visitor
visitBehaviorNameChange: aClassRenameChange
	(environment classOrTraitNamed: aClassRenameChange oldName)
		rename: aClassRenameChange newName