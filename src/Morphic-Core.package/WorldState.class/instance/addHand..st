hands
addHand: aHandMorph
	"Add the given hand to the list of hands for this world."

	hands := (hands copyWithout: aHandMorph) copyWith: aHandMorph.
