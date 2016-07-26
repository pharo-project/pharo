as yet unclassified
isHorizontal
	"Answer true if the receiver has a horizontal layout."
	
	^self edgeName == #top
		or: [self edgeName == #bottom]