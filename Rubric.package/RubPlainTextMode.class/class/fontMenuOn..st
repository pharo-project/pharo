menu
fontMenuOn: aBuilder
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubTextEditorShiftedMenu'
		model: nil) menu popUpInWorld"

	(aBuilder item: #'fontItems')
		label: 'Font...';
		withSeparatorAfter;
		with: [ 
					(aBuilder item: #'Set font...' translated)
						keyText: 'k';
						selector: #offerFontMenu.
					(aBuilder item: #'Set alignment...' translated)
						selector: #chooseAlignment;
						withSeparatorAfter ]