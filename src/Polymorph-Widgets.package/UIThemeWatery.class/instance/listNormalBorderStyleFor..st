border-styles
listNormalBorderStyleFor: aList
	"Return the normal borderStyle for the given list"

	^BorderStyle simple
		width: 1;
		baseColor: aList paneColor