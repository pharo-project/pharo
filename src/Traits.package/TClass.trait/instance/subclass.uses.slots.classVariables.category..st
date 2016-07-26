subclass creation - slots
subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol
	^ self
		subclass: aSubclassSymbol
		uses: aTraitCompositionOrArray
		with: [ 
			self classInstaller
				make: [ :builder | 
					builder
						name: aSubclassSymbol;
						superclass: self;
						slots: slotDefinition;
						sharedVariables: classVarDefinition;
						category: aCategorySymbol ] ]