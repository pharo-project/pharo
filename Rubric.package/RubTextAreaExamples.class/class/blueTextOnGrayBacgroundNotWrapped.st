examples
blueTextOnGrayBacgroundNotWrapped
	"self blueTextOnGrayBacgroundNotWrapped"

	RubEditingArea new
		backgroundColor: Color lightGray;
		updateTextWith: self smallLoremIpsum;
		textColor: Color blue;
		beNotWrapped; 
		width: 400;
		openInWorld