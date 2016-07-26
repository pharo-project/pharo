instance-creation
newHighlight
	"Anewser a new highlight."

	^TextHighlightByBounds new
		color: self modificationColor;
		borderWidth: 1;
		borderColor: self edgeColor