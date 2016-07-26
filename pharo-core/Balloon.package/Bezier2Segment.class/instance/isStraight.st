testing
isStraight
	"Return true if the receiver represents a straight line"
	^(self tangentAtStart crossProduct: self tangentAtEnd) = 0