border-styles
dropListNormalListBorderStyleFor: aDropList
	"Return the normal borderStyle for the list of the given given drop list"

	^BorderStyle inset
		width: 1;
		baseColor: self buttonColor lighter.