initialization
initialize

	debugger := nil asValueHolder.
	super initialize.
	
	self whenWindowChanged: [:w | w whenClosedDo: [ self clear ] ].
	self bindKeyCombination: Character escape asKeyCombination toAction: [ self delete ]