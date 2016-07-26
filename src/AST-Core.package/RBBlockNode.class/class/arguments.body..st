instance creation
arguments: argNodes body: sequenceNode 
	^(self new)
		arguments: argNodes;
		body: sequenceNode;
		yourself