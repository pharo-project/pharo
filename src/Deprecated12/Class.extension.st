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
		transformWith: '`@rcv ephemeronSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: EphemeronLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					package: `@arg4;
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
			'`@rcv ephemeronSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 category: `@arg4'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: EphemeronLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					category: `@arg4;
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
			'`@rcv ephemeronSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: EphemeronLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					package: `@arg4;
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
		transformWith: '`@rcv immediateSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: ImmediateLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					package: `@arg4;
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
			'`@rcv immediateSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 category: `@arg4'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: ImmediateLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					category: `@arg4;
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
			'`@rcv immediateSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: ImmediateLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					package: `@arg4;
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
		transformWith: '`@rcv subclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					category: `@arg4;
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
			'`@rcv subclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 category: `@arg4'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					category: `@arg4;
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
		transformWith: '`@rcv subclass: `@arg1 layout: `@arg2 slots: `@arg3 classVariables: `@arg4 package: `@arg5' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: `@arg2;
					slots: `@arg3;
					sharedVariables: `@arg4;
					package: `@arg5;
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
		transformWith: '`@rcv subclass: `@arg1 layout: `@arg2 slots: `@arg3 classVariables: `@arg4 poolDictionaries: `@arg6 package: `@arg5' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: `@arg2;
					slots: `@arg3;
					sharedVariables: `@arg4;
					sharedPools: `@arg6
					package: `@arg5;
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
		transformWith: '`@rcv subclass: `@arg1 slots: `@arg2 classVariables: `@arg3 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					slots: `@arg2;
					sharedVariables: `@arg3;
					package: `@arg4;
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
		transformWith: '`@rcv subclass: `@arg1 slots: `@arg2 classVariables: `@arg3 poolDictionaries: `@arg5 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					slots: `@arg2;
					sharedVariables: `@arg3;
					sharedPools: `@arg5
					package: `@arg4;
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
		transformWith: '`@rcv variableByteSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@arg1);
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					category: `@arg4;
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
		transformWith: '`@rcv variableByteSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@arg1);
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					package: `@arg4;
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
		transformWith: '`@rcv variableByteSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 category: `@arg4'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@arg1);
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					category: `@arg4;
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
			'`@rcv variableByteSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 package: `@arg4'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: (`@rcv checkForCompiledMethodLayout: `@arg1);
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					package: `@arg4;
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
		transformWith: '`@rcv variableDoubleWordSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: DoubleByteLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					package: `@arg4;
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
		transformWith: '`@rcv variableDoubleWordSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 package: `@arg4'
			-> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: DoubleWordLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					package: `@arg4;
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
		transformWith: '`@rcv variableSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: VariableLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					category: `@arg4;
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
		transformWith: '`@rcv variableSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: VariableLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					package: `@arg4;
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
			'`@rcv variableSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: VariableLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
			 		sharedPools: `@arg5;
					category: `@arg4;
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
			'`@rcv variableSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: VariableLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
			 		sharedPools: `@arg5;
					package: `@arg4;
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
		transformWith: '`@rcv variableWordSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: WordLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					category: `@arg4;
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
Class >> variableWordSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv variableWordSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: WordLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					package: `@arg4;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WordLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  package: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableWordSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 sharedPools: `@arg5 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: WordLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					category: `@arg4;
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
Class >> variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv variableWordSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 sharedPools: `@arg5 package: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: WordLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					sharedPools: `@arg5;
					package: `@arg4;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WordLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  package: cat;
			  environment: self environment ]
]

{ #category : '*Deprecated12' }
Class >> weakSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv weakSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: WeakLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
					category: `@arg4;
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
Class >> weakSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith:
			'`@rcv weakSubclass: `@arg1 instanceVariableNames: `@arg2 classVariableNames: `@arg3 poolDictionaries: `@arg5 category: `@arg4' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: `@rcv;
					name: `@arg1;
					layoutClass: WeakLayout;
					slotsFromString: `@arg2;
					sharedVariablesFromString: `@arg3;
			 		sharedPools: `@arg5;
					category: `@arg4;
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
