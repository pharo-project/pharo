Extension { #name : 'Class' }

{ #category : '*TraitsV2' }
Class >> setTraitComposition: aTraitComposition [

	^ self classInstaller
		  update: self
		  to: [ :builder | builder traitComposition: aTraitComposition ]
]

{ #category : '*TraitsV2' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray layout: layoutClass slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  layoutClass: layoutClass;
			  sharedVariables: classVarDefinition;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategorySymbol ]
]

{ #category : '*TraitsV2' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray layout: layoutClass slots: slotDefinition classVariables: classVarDefinition package: package [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  layoutClass: layoutClass;
			  sharedVariables: classVarDefinition;
			  traitComposition: aTraitCompositionOrArray;
			  package: package ]
]

{ #category : '*TraitsV2' }
Class >> subclass: subclassName uses: aTraitComposition layout: aLayout slots: someSlots classVariables: someClassVariables poolDictionaries: somePoolDictionaries category: aCategory [

	^ self classInstaller make: [ :builder |
		  builder
			  name: subclassName;
			  superclass: self;
			  slots: someSlots;
			  layoutClass: aLayout;
			  sharedVariables: someClassVariables;
			  sharedPools: somePoolDictionaries;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*TraitsV2' }
Class >> subclass: subclassName uses: aTraitComposition layout: aLayout slots: someSlots classVariables: someClassVariables poolDictionaries: somePoolDictionaries package: package [

	^ self classInstaller make: [ :builder |
		  builder
			  name: subclassName;
			  superclass: self;
			  slots: someSlots;
			  layoutClass: aLayout;
			  sharedVariables: someClassVariables;
			  sharedPools: somePoolDictionaries;
			  traitComposition: aTraitComposition;
			  package: package ]
]

{ #category : '*TraitsV2' }
Class >> subclass: subclassName uses: aTraitComposition layout: aLayout slots: someSlots classVariablesNames: someClassVariablesNames poolDictionaries: somePoolDictionaries category: aCategory [

	^ self classInstaller make: [ :builder |
		  builder
			  name: subclassName;
			  superclass: self;
			  slots: someSlots;
			  layoutClass: aLayout;
			  sharedVariablesFromString: someClassVariablesNames;
			  sharedPools: somePoolDictionaries;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*TraitsV2' }
Class >> subclass: subclassName uses: aTraitComposition layout: aLayout slots: someSlots classVariablesNames: someClassVariablesNames poolDictionaries: somePoolDictionaries package: package [

	^ self classInstaller make: [ :builder |
		  builder
			  name: subclassName;
			  superclass: self;
			  slots: someSlots;
			  layoutClass: aLayout;
			  sharedVariablesFromString: someClassVariablesNames;
			  sharedPools: somePoolDictionaries;
			  traitComposition: aTraitComposition;
			  package: package ]
]

{ #category : '*TraitsV2' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategorySymbol ]
]

{ #category : '*TraitsV2' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition package: package [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  traitComposition: aTraitCompositionOrArray;
			  package: package ]
]

{ #category : '*TraitsV2' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  sharedPools: someSharedPoolNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategorySymbol ]
]

{ #category : '*TraitsV2' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames package: package [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  sharedPools: someSharedPoolNames;
			  traitComposition: aTraitCompositionOrArray;
			  package: package ]
]

{ #category : '*TraitsV2' }
Class >> usesTrait: aTrait [
	"Returns whether self or one of its superclasses are among the users of aTrait"
	^ aTrait users includesAny: self withAllSuperclasses
]

{ #category : '*TraitsV2' }
Class >> usesTraitLocally: aTrait [
	"Returns whether self is among the users of aTrait. Note that this will return false if aTrait is used by a superclass of self and not self itself. If you want to get true in this case, use #usesTrait:"
	^ aTrait users includes: self
]
