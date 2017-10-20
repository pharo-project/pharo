private-changes
renameInstanceVariable: oldName to: newName in: aRBClass around: aBlock 
	^self performChange: (RBRefactoryChangeManager changeFactory 
				renameInstanceVariable: oldName
				to: newName
				in: aRBClass)
		around: aBlock