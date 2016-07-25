halos and balloon help
balloonHelpAligner
	"Answer the morph to which the receiver's balloon help should point"
	^ (self valueOfProperty: #balloonTarget) ifNil: [self]