converting
asUndoOperation
	^ RBRefactoryChangeManager changeFactory
		removeClassVariable: self variable
		from: self changeClass