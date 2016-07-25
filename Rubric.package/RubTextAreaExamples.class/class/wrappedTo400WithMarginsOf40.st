examples
wrappedTo400WithMarginsOf40
	"self wrappedTo400WithMarginsOf40"

	RubEditingArea new
		beWrapped;
		width: 400;
		updateMarginsWith: (40@40 corner: 40@40);
		updateTextWith: self smallLoremIpsum;
		openInWorld