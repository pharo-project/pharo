instance-creation
newHighlight: type
	"Anewser a new highlight."

	^TextHighlightByBounds new
		color: (self colorForType: type);
		borderWidth: 1;
		borderColor: self edgeColor;
		fillWidth: true