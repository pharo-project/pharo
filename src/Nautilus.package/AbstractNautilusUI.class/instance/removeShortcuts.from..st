shortcuts
removeShortcuts: groupSymbol from: aMorph
	Nautilus useOldStyleKeys ifFalse: [ ^self ].
	
	(aMorph kmDispatcher includesKeymapCategory: groupSymbol)
		ifTrue: [ 
			aMorph 
				detachKeymapCategory: groupSymbol
				targetting: self ]
