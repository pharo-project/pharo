world state
removeHand: aHandMorph
	"Remove the given hand from the list of hands for this world."

	(worldState hands includes: aHandMorph)
		ifFalse: [ ^ self ].
	aHandMorph dropMorphs.
	self invalidRect: aHandMorph fullBounds.
	worldState removeHand: aHandMorph