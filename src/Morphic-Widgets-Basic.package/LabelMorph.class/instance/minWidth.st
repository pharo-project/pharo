layout
minWidth
	"Answer the minmum width of the receiver.
	Based on font and contents."

	^self valueOfProperty: #minWidth ifAbsent: [self measureContents x] "allow override"