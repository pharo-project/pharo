examples
blueTextOnGrayBacgroundWrappedTo400Version2
	"self blueTextOnGrayBacgroundWrappedTo400Version2"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: ((Text fromString: self smallLoremIpsum) addAttribute: (TextColor color: Color blue));
		backgroundColor: Color lightGray;
		openInWorld