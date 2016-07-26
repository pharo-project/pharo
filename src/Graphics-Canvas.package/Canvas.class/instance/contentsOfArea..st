accessing
contentsOfArea: aRectangle
	"Return the contents of the given area"
	^self contentsOfArea: aRectangle into: (Form extent: aRectangle extent depth: self depth)