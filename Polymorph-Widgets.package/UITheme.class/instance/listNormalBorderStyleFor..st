border-styles
listNormalBorderStyleFor: aList
	"Return the normal borderStyle for the given list"

	^BorderStyle inset
		width: 1;
		baseColor: aList paneColor