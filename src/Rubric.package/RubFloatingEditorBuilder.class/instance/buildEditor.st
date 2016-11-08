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

	"until we have better a rub text editor for text field like components"
	editor textArea on: PharoShortcuts current findShortcut do:[:x | "nothing, we don't need a search within a floating editor widget"].
	editor textArea withoutSelectionBar.
	^ self customizedEditor