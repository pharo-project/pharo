examples
smallLoremIpsumWrappedTo400
	"self smallLoremIpsumWrappedTo400"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: self smallLoremIpsum;
		openInWorld