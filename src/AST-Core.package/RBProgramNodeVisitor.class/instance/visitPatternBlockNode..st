visiting
visitPatternBlockNode: aRBPatternBlockNode 
	self visitArgumentNodes: aRBPatternBlockNode arguments.
	self visitNode: aRBPatternBlockNode body