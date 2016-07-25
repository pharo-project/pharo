initialization
addMorphsWithOptions

	self 
		addMainMorphsWith: optionsPanel height.

	self
		addMorph: optionsPanel 
		fullFrame: (LayoutFrame identity
			bottomFraction: 0;
			bottomOffset: optionsPanel height)