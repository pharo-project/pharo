testing
isFlexed
	"Return true if the receiver is currently flexed"
	^ owner notNil and: [owner isFlexMorph]