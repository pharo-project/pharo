shortcuts
setShortcuts: groupSymbol to: aMorph
	aMorph 
		attachKeymapCategory: (	Nautilus useOldStyleKeys 
			ifTrue: [ groupSymbol ]
			ifFalse: [ #NautilusGlobalShortcuts ])
		targetting: self
