visitor-double dispatching
visitSequenceNode: aSequenceNode
	aSequenceNode temporaries: (self visitTemporaryNodes: aSequenceNode temporaries).
	aSequenceNode statements: (aSequenceNode statements collect: [ :each | self visitNode: each ])