world state
addHand: aHandMorph
	"Add the given hand to the list of hands for this world."

	aHandMorph owner ifNotNil: [ aHandMorph owner removeHand: aHandMorph ].
	worldState addHand: aHandMorph.
	aHandMorph privateOwner: self