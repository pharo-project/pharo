examples
blueTextOnGrayBacgroundWrappedTo400
	"self blueTextOnGrayBacgroundWrappedTo400"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: self smallLoremIpsum;
		backgroundColor: Color lightGray;
		textColor: Color blue;
		font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 20);
		openInWorld