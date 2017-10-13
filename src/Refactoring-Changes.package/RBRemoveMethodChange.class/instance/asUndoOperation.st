converting
asUndoOperation
	^ RBRefactoryChangeManager changeFactory addMethodSource: (self methodSourceFor: selector) in: self changeClass