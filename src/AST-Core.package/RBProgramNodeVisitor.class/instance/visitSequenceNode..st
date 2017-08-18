visiting
visitSequenceNode: aSequenceNode 
	self visitTemporaryNodes: aSequenceNode temporaries.
	aSequenceNode statements do: [:each | self visitNode: each]