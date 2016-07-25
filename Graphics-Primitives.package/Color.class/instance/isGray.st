queries
isGray
	"Return true if the receiver represents a shade of gray"
	^(self privateRed = self privateGreen) and: [self privateRed = self privateBlue]