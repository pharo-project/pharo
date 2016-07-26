choosing
chooseStandardFont: aSelector 
	self
		chooseFontWithTitle: (self systemFontLabelOf: aSelector)	, '...'
		for: self
		setSelector: (aSelector , ':') asSymbol
		getSelector: aSelector