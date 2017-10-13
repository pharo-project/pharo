helpers
critiqueFor: aMessageNode
	^ (super critiqueFor: aMessageNode)
		tinyHint: aMessageNode selector;
		yourself