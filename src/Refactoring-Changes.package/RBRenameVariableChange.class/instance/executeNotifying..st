private
executeNotifying: aBlock 
	| undo |
	self addNewVariable.
	self copyOldValuesToNewVariable.
	undo := super executeNotifying: aBlock.
	undo
		oldName: newName;
		newName: oldName.
	self removeOldVariable.
	^ undo