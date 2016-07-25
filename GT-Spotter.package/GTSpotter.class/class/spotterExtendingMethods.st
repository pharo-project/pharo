inspecting
spotterExtendingMethods
	^(SystemNavigation default allBehaviors
		flatCollect: [ :each | 
			(Pragma allNamed: self extensionsPragma in: each) 
				collect: [ :p | p method asRingDefinition ] ])
		sorted