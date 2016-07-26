accessing
descent
	"Answer the receiver's maximum extent of characters below the baseline."

	^pointSize < 9
		ifTrue: [descent-1]
		ifFalse: [descent]