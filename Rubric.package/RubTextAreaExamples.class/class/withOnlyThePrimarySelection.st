examples
withOnlyThePrimarySelection
	"self withOnlyThePrimarySelection"

	ModelForShout := nil.
	RubEditingArea new
		beWrapped;
		width: 400;
		model: self;
		withoutAnyDecorator;
		updateTextWith: self smallLoremIpsum;
		openInWorld.
		
