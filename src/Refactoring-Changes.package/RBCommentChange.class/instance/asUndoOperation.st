converting
asUndoOperation
	^ self copy
		comment: self changeClass organization classComment;
		yourself