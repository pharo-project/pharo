instance creation
selector: aSymbol arguments: variableNodes body: aSequenceNode 
	^(self new)
		arguments: variableNodes;
		selector: aSymbol;
		body: aSequenceNode;
		yourself