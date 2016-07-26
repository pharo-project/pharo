subclass creation - slots
subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol

	^ self classInstaller make: [ :builder |
		builder name: aSubclassSymbol;
		superclass: self;
		slots: slotDefinition;
		sharedVariables:  classVarDefinition;
		sharedPools: someSharedPoolNames;
		category: aCategorySymbol ].
