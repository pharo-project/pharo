initialize
registerShortcuts

	self 
		bindKeyCombination: $] meta shift toAction: [ self selectNext ];
		bindKeyCombination: $[ meta shift toAction: [ self selectPrevious ];
		bindKeyCombination: $w command toAction: [ self deleteSelectedTabs ];
		bindKeyCombination: $w command shift toAction: [ self closeAllTabs ].
		
	1 to: 9 do: [ :index |
		self bindKeyCombination: index asString first meta toAction: [ self selectTabAt: index ] ].
	
	self bindKeyCombination: $0 meta toAction: [ self selectLastTab ]