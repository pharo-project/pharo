debug and other
addDebuggingItemsTo: aMenu hand: aHandMorph
	aMenu add: 'debug...' subMenu: (self buildDebugMenu: aHandMorph).
	aMenu lastItem icon: (self iconNamed: #smallDebugIcon)