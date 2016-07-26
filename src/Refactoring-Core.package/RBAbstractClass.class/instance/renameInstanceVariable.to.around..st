variable accessing
renameInstanceVariable: oldName to: newName around: aBlock 
	self privateInstanceVariableNames 
		at: (self privateInstanceVariableNames indexOf: oldName asString)
		put: newName asString.
	model 
		renameInstanceVariable: oldName
		to: newName
		in: self
		around: aBlock