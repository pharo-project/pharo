morphic ui
listMorph: listSymbol selection: selectionSymbol menu: menuSymbol keystroke: keystrokeSymbol
	^ (PluggableListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol)
		keystrokeActionSelector: keystrokeSymbol;
		yourself