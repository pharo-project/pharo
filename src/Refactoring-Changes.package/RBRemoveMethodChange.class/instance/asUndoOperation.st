converting
asUndoOperation
	^ RBAddMethodChange compile: (self methodSourceFor: selector) in: self changeClass