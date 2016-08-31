*GT-InspectorExtensions-Core
pragmas
	^ (SystemNavigation default allBehaviors flatCollect: [ :each | 
		(Pragma allNamed: self in: each) 
			collect: [ :p | p method asRingDefinition ] ])