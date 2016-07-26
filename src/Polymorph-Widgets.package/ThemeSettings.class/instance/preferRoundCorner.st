accessing
preferRoundCorner
	"Answer the value of preferRoundCorner"

	^preferRoundCorner  ifNil: [preferRoundCorner := self preferRoundCornerByDefault]