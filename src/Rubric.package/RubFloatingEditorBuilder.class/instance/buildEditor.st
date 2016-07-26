private
buildEditor
	editor := RubScrolledTextMorph new forbidMenu.
	editor
		autoAccept: autoAccept;
		setTextWith: self initialContents.
	World activeHand addEventListener: self.
	editor announcer
		when: MorphLostFocus send: #whenEditorLostFocus: to: self;
		when: RubTextAccepted send: #whenTextAccepted: to: self;
		when: RubReturnEntered send: #whenReturnEntered: to: self;
		when: RubKeystroke send: #whenKeystroke: to: self;
		when: MorphDeleted send: #whenEditorDeleted: to: self.
	editor
		beWrapped;
		font: font;
		margins: (2 @ 0 corner: 2 @ 1);
		selectFrom: initialContents size + 1 to: initialContents size.
	editor textArea withoutSelectionBar.
	^ self customizedEditor