visiting
visitVariableNode: aVariableNode
	super visitVariableNode: aVariableNode.
	self flag: #TBD. "needs to be extendend to other kinds of variables and cleaned"
	aVariableNode isGlobal ifTrue: [  
		self analyseForLinksForVariables: aVariableNode ].
	(aVariableNode isInstance) ifTrue: [  
		self analyseForLinksForVariables: aVariableNode ]