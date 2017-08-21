subclass creation - slots
subclass: aSubclassSymbol  layout: layoutClass slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol

	^ self classInstaller make: [ :builder |
		builder name: aSubclassSymbol;
		superclass: self;
		layoutClass: layoutClass;
		slots: slotDefinition;
		sharedVariables: classVarDefinition;
		category: aCategorySymbol ].
