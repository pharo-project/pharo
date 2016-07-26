initialization
initializeWidgets

	categoryList := self newDropList.
	shortcutList := self newMultiColumnList.
	okToolbar := self instantiate: OkToolbar.
	orderingList := self newDropList.
	
	orderingList 
		addItemLabeled:'shortcut	:	description' 
			do: [self collectShortcutDescription.
				 self updateShortcutList];
		addItemLabeled:'description	:	shortcut' 
			do: [self collectDescriptionShortcut.
				 self updateShortcutList].
	
	shortcutList 
		displayBlock: [ :e | e ];
		allowToSelect: false.

	okToolbar okButton
			label: 'Close';
			state: false