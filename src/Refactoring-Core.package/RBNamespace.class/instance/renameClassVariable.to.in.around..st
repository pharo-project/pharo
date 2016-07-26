private-changes
renameClassVariable: oldName to: newName in: aRBClass around: aBlock 
	^self performChange: (RBRenameClassVariableChange 
				rename: oldName
				to: newName
				in: aRBClass)
		around: aBlock