adding nodes
addTemporaryNamed: aString 
	| variableNode |
	variableNode := RBVariableNode named: aString.
	variableNode parent: self.
	temporaries := temporaries copyWith: variableNode.
	^ variableNode