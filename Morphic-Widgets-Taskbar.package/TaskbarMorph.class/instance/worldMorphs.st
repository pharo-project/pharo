taskbar
worldMorphs
	"Answer the world's submorphs plus those in hand.
	Nasty case since hand removes the morph before dropping"
	
	^self world submorphs, 
		((self tasks
			select: [:t | t morph owner = self world activeHand])
			collect: [:t | t morph])