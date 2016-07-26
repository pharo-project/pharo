morphic ui
widgetSpecs
	 ^#(
		((buttonRow) (0 0 1 0) (0 0 0 30))
		((multiListMorph:selection:listSelection:menu: list selection listSelectionAt: methodListMenu:) (0 0 1 0.4) (0 30 0 0))
		((buttonRow: #(('Select All' selectAll 'select all changes') ('Select None' selectNone 'select no changes'))) (0 0.4 1 0.4) (0 0 0 30))
		((textMorph: text) (0 0.4 1 1) (0 30 0 0))
		)

	