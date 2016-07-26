morphic ui
listMorph: listSymbol keystroke: keystrokeSymbol
	^ (self
		listMorph: (listSymbol, 'List') asSymbol
		selection: (listSymbol, 'Selection') asSymbol
		menu: (listSymbol, 'ListMenu:') asSymbol)
		keystrokeActionSelector: keystrokeSymbol;
		yourself