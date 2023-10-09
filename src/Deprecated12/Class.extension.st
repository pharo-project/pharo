Extension { #name : 'Class' }

{ #category : '*Deprecated12' }
Class >> basicCategory [

	self deprecated: 'Use #category now' transformWith: '`@rcv basicCategory' -> '`@rcv category'.
	^ self category
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
					sharedPools: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: EphemeronLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
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
					sharedPools: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: EphemeronLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
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
					sharedPools: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: ImmediateLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
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
					sharedPools: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: ImmediateLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  package: package;
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
			  sharedPools: s;
			  category: cat;
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
			  sharedPools: someSharedPoolNames;
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
			  sharedPools: someSharedPoolNames;
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
					sharedPools: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: (self checkForCompiledMethodLayout: className);
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
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
					sharedPools: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: (self checkForCompiledMethodLayout: className);
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  package: package;
			  environment: self environment ]
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
			 		sharedPools: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  sharedPools: s;
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
			 		sharedPools: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: VariableLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  package: package;
			  environment: self environment ]
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
					sharedPools: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WordLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
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
					sharedPools: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WordLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  package: package;
			  environment: self environment ]
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
			 		sharedPools: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WeakLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
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
					sharedPools: `@pools;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WeakLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  category: package;
			  environment: self environment ]
]
