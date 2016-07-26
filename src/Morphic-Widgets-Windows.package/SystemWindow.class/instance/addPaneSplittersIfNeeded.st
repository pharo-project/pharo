activation
addPaneSplittersIfNeeded

	(isCollapsed not 
		and: [ self paneMorphs isNotEmpty and: [self splitters isEmpty]])
			ifTrue: [self addPaneSplitters].