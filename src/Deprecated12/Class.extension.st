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
