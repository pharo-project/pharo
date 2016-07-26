morphic ui
listMorph: listSymbol selection: selectionSymbol menu: menuSymbol
	^ PluggableListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol