subclass creation - slots
subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol

	^ self classInstaller make: [ :builder | 
			builder
				name: aSubclassSymbol;
				superclass: self;
				slots: slotDefinition;
				sharedVariables: classVarDefinition;
				category: aCategorySymbol ]