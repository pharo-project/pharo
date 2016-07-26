menu declaration
textEditorShiftedMenuOn: aBuilder 
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'textEditorShiftedMenu'
		model: nil) menu popUpInWorld"

	<contextMenu>
	<textEditorShiftedMenu>
	(aBuilder item: #'Set font...' translated) 
		keyText: 'k';
		selector: #offerFontMenu.
	(aBuilder item: #'Set style... (K)' translated) 
		keyText: 'K';
		selector: #changeStyle.
	(aBuilder item: #'Set alignment...' translated) 
		selector: #chooseAlignment.
	(aBuilder item: #'More...' translated) 
		selector: #yellowButtonActivity.
