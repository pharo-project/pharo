morphic ui
treeMorph: listSymbol selection: selectionSymbol menu: menuSymbol
	^ SimpleHierarchicalListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol
		keystroke: nil