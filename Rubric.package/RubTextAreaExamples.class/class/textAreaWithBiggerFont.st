examples
textAreaWithBiggerFont
	" self textAreaWithBiggerFont "

	RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: 'test';
		takeKeyboardFocus;
		onAnnouncement: MorphLostFocus do: [ :ann | ann morph delete ];
		font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 20);
		openCenteredInWorld