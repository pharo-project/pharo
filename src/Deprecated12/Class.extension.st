Extension { #name : 'Class' }

{ #category : '*Deprecated12' }
Class >> anyUserOfClassVarNamed: aSymbol [
	self deprecated: 'use #usingClasses on the Variable instead'.
	^ (self classVariableNamed: aSymbol) usingClasses
		ifEmpty: [ nil ]
		ifNotEmpty: [:notEmpty | notEmpty anyOne]
]

{ #category : '*Deprecated12' }
Class >> basicCategory [

	self deprecated: 'Use #category now' transformWith: '`@rcv basicCategory' -> '`@rcv category'.
	^ self category
]

{ #category : '*Deprecated12' }
Class >> category: aString [

	| converter |
	self deprecated: 'Trying to package a class using the concept of categpry is not supported anymore in Pharo. You can use #package:tag: instead to avoid any ambiguity on what part of the category is the package and what is the tag.'.
	self category = aString ifTrue: [ ^ self ].

	converter := CategoryConverter category: aString asSymbol.

	^ self package: converter packageName tag: converter tagName
]

{ #category : '*Deprecated12' }
Class >> ephemeronSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [
	"Added to allow for a simplified subclass creation experience. "

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv ephemeronSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: EphemeronLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: EphemeronLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> ephemeronSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv ephemeronSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: EphemeronLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: EphemeronLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> ephemeronSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: package [
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables."

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv ephemeronSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: EphemeronLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: EphemeronLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> immediateSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv immediateSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: ImmediateLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: ImmediateLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv immediateSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: ImmediateLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: ImmediateLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv immediateSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: ImmediateLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: ImmediateLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> immediateSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv immediateSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: ImmediateLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aClassName;
			  superclass: self;
			  slotsFromString: instVarNameList;
			  layoutClass: ImmediateLayout;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> immediateSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv immediateSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: ImmediateLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: someInstanceVariableNames;
			  layoutClass: ImmediateLayout;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitComposition;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> removeFromChanges [

	self deprecated: 'The ChangeSet system is been removed in Pharo 12 since Epicea replaces all its features already.'
]

{ #category : '*Deprecated12' }
Class >> subclass: t instanceVariableNames: ins [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name instanceVariableNames: `@instVar' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  slotsFromString: ins;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol instanceVariableNames: instVarNameList classVariableNames: classVarNames category: aCategorySymbol [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: aSubclassSymbol;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: aCategorySymbol;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol instanceVariableNames: instVarNameList classVariableNames: classVarNames package: aPackageSymbol [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: aSubclassSymbol;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: aPackageSymbol;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPools: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPools: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: self classLayout class;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol layout: layoutClass slots: slotDefinition classVariables: classVarDefinition package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name layout: `@layout slots: `@instVar classVariables: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: `@layout;
					slots: `@instVar;
					sharedVariables: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  layoutClass: layoutClass;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol layout: layoutClass slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name layout: `@layout slots: `@instVar classVariables: `@classVar poolDictionaries: `@arg6 package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: `@layout;
					slots: `@instVar;
					sharedVariables: `@classVar;
					sharedPools: `@arg6
					package: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  layoutClass: layoutClass;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  sharedPoolsFromString: someSharedPoolNames;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name slots: `@instVar classVariables: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slots: `@instVar;
					sharedVariables: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name slots: `@instVar classVariables: `@classVar poolDictionaries: `@pools package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slots: `@instVar;
					sharedVariables: `@classVar;
					sharedPools: `@pools
					package: `@package;
					environment: `@rcv environment ]'.


	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  sharedPoolsFromString: someSharedPoolNames;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aName uses: aTraitComposition [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name uses: `@trait' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					traitComposition: `@trait;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  traitComposition: aTraitComposition ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aName uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aName uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aName uses: aTraitCompositionOrArray instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: someInstanceVariableNames;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aName uses: aTraitCompositionOrArray instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: someInstanceVariableNames;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitCompositionOrArray;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray layout: layoutClass slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait layout: `@layout slots: `@instVar classVariables: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: `@layout;
					slots: `@instVar;
					sharedVariables: `@classVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
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

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray layout: layoutClass slots: slotDefinition classVariables: classVarDefinition package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait layout: `@layout slots: `@instVar classVariables: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: `@layout;
					slots: `@instVar;
					sharedVariables: `@classVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
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

{ #category : '*Deprecated12' }
Class >> subclass: subclassName uses: aTraitComposition layout: aLayout slots: someSlots classVariables: someClassVariables poolDictionaries: somePoolDictionaries category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait layout: `@layout slots: `@instVar classVariables: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: `@layout;
					slots: `@instVar;
					sharedVariables: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: subclassName;
			  superclass: self;
			  slots: someSlots;
			  layoutClass: aLayout;
			  sharedVariables: someClassVariables;
			  sharedPoolsFromString: somePoolDictionaries;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> subclass: subclassName uses: aTraitComposition layout: aLayout slots: someSlots classVariables: someClassVariables poolDictionaries: somePoolDictionaries package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait layout: `@layout slots: `@instVar classVariables: `@classVar poolDictionaries: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: `@layout;
					slots: `@instVar;
					sharedVariables: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: subclassName;
			  superclass: self;
			  slots: someSlots;
			  layoutClass: aLayout;
			  sharedVariables: someClassVariables;
			  sharedPoolsFromString: somePoolDictionaries;
			  traitComposition: aTraitComposition;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: subclassName uses: aTraitComposition layout: aLayout slots: someSlots classVariablesNames: someClassVariablesNames poolDictionaries: somePoolDictionaries category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait layout: `@layout slots: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: `@layout;
					slots: `@instVar;
					sharedVariables: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: subclassName;
			  superclass: self;
			  slots: someSlots;
			  layoutClass: aLayout;
			  sharedVariablesFromString: someClassVariablesNames;
			  sharedPoolsFromString: somePoolDictionaries;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> subclass: subclassName uses: aTraitComposition layout: aLayout slots: someSlots classVariablesNames: someClassVariablesNames poolDictionaries: somePoolDictionaries package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait layout: `@layout slots: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: `@layout;
					slots: `@instVar;
					sharedVariables: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: subclassName;
			  superclass: self;
			  slots: someSlots;
			  layoutClass: aLayout;
			  sharedVariablesFromString: someClassVariablesNames;
			  sharedPoolsFromString: somePoolDictionaries;
			  traitComposition: aTraitComposition;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name uses: `@trait slots: `@instVar classVariables: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slots: `@instVar;
					sharedVariables: `@classVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategorySymbol ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name uses: `@trait slots: `@instVar classVariables: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slots: `@instVar;
					sharedVariables: `@classVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  traitComposition: aTraitCompositionOrArray;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait slots: `@instVar classVariables: `@classVar poolDictionaries: `@pools category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slots: `@instVar;
					sharedVariables: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategorySymbol ]
]

{ #category : '*Deprecated12' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv subclass: `@name uses: `@trait slots: `@instVar classVariables: `@classVar poolDictionaries: `@pools package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					slots: `@instVar;
					sharedVariables: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitCompositionOrArray;
			  package: package ]
]

{ #category : '*Deprecated12' }
Class >> variableByteSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableByteSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@name);
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: (self checkForCompiledMethodLayout: className);
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableByteSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableByteSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@name);
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: (self checkForCompiledMethodLayout: className);
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableByteSubclass: className instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableByteSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@name);
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: (self checkForCompiledMethodLayout: className);
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableByteSubclass: className instanceVariableNames: f classVariableNames: d poolDictionaries: s package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableByteSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@name);
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: (self checkForCompiledMethodLayout: className);
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableByteSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableByteSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@name);
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: className;
			  superclass: self;
			  slotsFromString: instVarNameList;
			  layoutClass: (self checkForCompiledMethodLayout: className);
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableByteSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableByteSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@name);
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: someInstanceVariableNames;
			  layoutClass: (self checkForCompiledMethodLayout: aName);
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableDoubleByteSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableDoubleWordSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: DoubleByteLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: DoubleByteLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableDoubleWordSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableDoubleWordSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: DoubleWordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPools: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: DoubleWordLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: VariableLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: VariableLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: VariableLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
			 		sharedPoolsFromString: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  sharedPoolsFromString: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: VariableLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
			 		sharedPoolsFromString: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: VariableLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: VariableLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aClassName;
			  superclass: self;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames package: aCategory [
self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: VariableLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aClassName;
			  superclass: self;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: VariableLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
			 		sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  layoutClass: VariableLayout;
			  slotsFromString: someInstanceVariableNames;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: VariableLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
			 		sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  layoutClass: VariableLayout;
			  slotsFromString: someInstanceVariableNames;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableWordSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WordLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableWordSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WordLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableWordSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar sharedPools: `@pools category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WordLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableWordSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar sharedPools: `@pools package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WordLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  package: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableWordSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: className;
			  superclass: self;
			  slotsFromString: instVarNameList;
			  layoutClass: WordLayout;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames package: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableWordSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: className;
			  superclass: self;
			  slotsFromString: instVarNameList;
			  layoutClass: WordLayout;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableWordSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar sharedPools: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: someInstanceVariableNames;
			  layoutClass: WordLayout;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableWordSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar sharedPools: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WordLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: someInstanceVariableNames;
			  layoutClass: WordLayout;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv weakSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WeakLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WeakLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: package [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv weakSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WeakLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					package: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WeakLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv weakSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WeakLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
			 		sharedPoolsFromString: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WeakLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: package [
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables."

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv weakSubclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar sharedPools: `@pools package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WeakLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WeakLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPoolsFromString: s;
			  category: package;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv weakSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar category: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WeakLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: className;
			  superclass: self;
			  slotsFromString: instVarNameList;
			  layoutClass: WeakLayout;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNameList classVariableNames: classVarNames package: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv weakSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WeakLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: className;
			  superclass: self;
			  slotsFromString: instVarNameList;
			  layoutClass: WeakLayout;
			  sharedVariablesFromString: classVarNames;
			  traitComposition: aTraitCompositionOrArray;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv weakSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WeakLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
			 		sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: someInstanceVariableNames;
			  layoutClass: WeakLayout;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv weakSubclass: `@name uses: `@trait instanceVariableNames: `@instVar classVariableNames: `@classVar sharedPools: `@pools package: `@package'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@name;
					layoutClass: WeakLayout;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  superclass: self;
			  slotsFromString: someInstanceVariableNames;
			  layoutClass: WeakLayout;
			  sharedVariablesFromString: someClassVariableNames;
			  sharedPoolsFromString: someSharedPoolNames;
			  traitComposition: aTraitComposition;
			  category: aCategory ]
]
