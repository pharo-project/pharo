*Polymorph-Widgets
isEmbedded
	"Answer whether the receiver is embedded, i.e.
	has an owner that is not the world or the hand."

	^self owner isNotNil and: [self owner ~= self world and: [self owner ~= self activeHand]]