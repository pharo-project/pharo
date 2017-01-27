helpers
critiqueFor: aNode
	^ (super critiqueFor: aNode)
		tinyHint: aNode name;
		yourself