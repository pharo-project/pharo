subclass creation - slots
subclass: aSubclassSymbol  layout: layoutClass  slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol

	^ self classInstaller make: [ :builder |
		builder name: aSubclassSymbol;
		superclass: self;
		layoutClass: layoutClass;
		slots: slotDefinition;
		sharedVariables:  classVarDefinition;
		sharedPools: someSharedPoolNames;
		category: aCategorySymbol ].
