morphic ui
listMorph: listSymbol selection: selectionSymbol
	^ PluggableListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol