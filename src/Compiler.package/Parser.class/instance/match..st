scanning
match: type 
	"Answer with true if next tokens type matches."

	hereType == type
		ifTrue: 
			[self advance.
			^true].
	^false