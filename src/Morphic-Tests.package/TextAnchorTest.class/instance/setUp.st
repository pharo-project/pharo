running
setUp
	super setUp.
	prototypes
		add: (TextAnchor new anchoredMorph: Morph new initialize);
		
		add: (TextAnchor new anchoredMorph: EllipseMorph new initialize) 