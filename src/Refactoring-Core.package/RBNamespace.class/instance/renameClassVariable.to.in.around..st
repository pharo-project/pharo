private-changes
renameClassVariable: oldName to: newName in: aRBClass around: aBlock 
	^self performChange: (RBRefactoryChangeManager changeFactory  
				renameClassVariable: oldName
				to: newName
				in: aRBClass)
		around: aBlock