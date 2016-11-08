menu declaration
textEditorMenuOn: aBuilder
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'textEditorMenu'
		model: nil) menu popUpInWorld"

	<contextMenu>
	<textEditorMenu>
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		iconName: #smallFindIcon.
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		iconName: #smallFindIcon.
	(aBuilder item: #'Set search string' translated)
		keyText: 'h';
		selector: #setSearchString.
	(aBuilder item: #'Do again' translated)
		keyText: 'j';
		selector: #again;
		iconNamed: #smallRedoIcon.
	(aBuilder item: #Undo translated)
		keyText: 'z';
		selector: #undo;
		iconName: #smallUndoIcon;
		withSeparatorAfter.
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		iconName: #smallCopyIcon.
	(aBuilder item: #Cut translated)
		keyText: 'x';
		selector: #cut;
		iconName: #smallCutIcon.
	(aBuilder item: #Paste translated)
		keyText: 'v';
		selector: #paste;
		iconName: #smallPasteIcon.
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		iconName: #smallCopyIcon;
		withSeparatorAfter.
	(aBuilder item: #'Set font...' translated)
		keyText: 'k';
		selector: #offerFontMenu.
	(aBuilder item: #'Set style...' translated)
		keyText: 'K';
		selector: #changeStyle.
	(aBuilder item: #'Set alignment...' translated)
		selector: #chooseAlignment;
		withSeparatorAfter.
	(aBuilder item: #Accept translated)
		keyText: 's';
		selector: #accept;
		iconName: #smallOkIcon.
	(aBuilder item: #Cancel translated)
		keyText: 'l';
		selector: #cancel;
		iconName: #smallCancelIcon