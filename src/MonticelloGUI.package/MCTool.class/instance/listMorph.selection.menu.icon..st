morphic ui
listMorph: listSymbol selection: selectionSymbol menu: menuSymbol icon: iconSelector
	^ (PluggableIconListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol)
			getIconSelector: iconSelector;
			yourself