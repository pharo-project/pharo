hands
removeHand: aHandMorph
	"Remove the given hand from the list of hands for this world."

	(hands includes: aHandMorph) ifFalse: [^self].
	hands := hands copyWithout: aHandMorph.
	activeHand == aHandMorph ifTrue: [activeHand := nil].
	ActiveHand == aHandMorph ifTrue: [ActiveHand := nil].
