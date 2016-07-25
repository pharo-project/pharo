border-styles
dropListNormalBorderStyleFor: aDropList
	"Return the normal borderStyle for the given drop list"

	^BorderStyle inset
		width: 1;
		baseColor: aDropList paneColor