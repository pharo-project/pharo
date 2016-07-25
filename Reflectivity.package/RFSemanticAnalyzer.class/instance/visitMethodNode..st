visiting
visitMethodNode: aMethodNode
	
	scope := compilationContext scope newMethodScope. 
	aMethodNode scope: scope.  scope node: aMethodNode.
	aMethodNode arguments do: [:node | self declareArgumentNode: node ].
	aMethodNode pragmas do: [:each | self visitNode: each].
	self analyseForLinksForNodes: aMethodNode.
	self visitNode: aMethodNode body.
	scope := scope outerScope.
