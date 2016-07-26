private-changes
renameInstanceVariable: oldName to: newName in: aRBClass around: aBlock 
	^self performChange: (RBRenameInstanceVariableChange 
				rename: oldName
				to: newName
				in: aRBClass)
		around: aBlock