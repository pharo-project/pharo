private
rebuildFieldAccessors
	self removeAllOffsetVariables. 
	self 
		compileFields: self fields 
		withAccessors: true