variable accessing
renameClassVariable: oldName to: newName around: aBlock 
	self privateClassVariableNames 
		at: (self privateClassVariableNames indexOf: oldName asSymbol)
		put: newName asSymbol.
	model 
		renameClassVariable: oldName
		to: newName
		in: self
		around: aBlock