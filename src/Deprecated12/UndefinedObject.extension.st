Extension { #name : 'UndefinedObject' }

{ #category : '*Deprecated12' }
UndefinedObject >> subclass: nameOfClass instanceVariableNames: instVarNameList classVariableNames: classVarNames poolDictionaries: poolDictnames category: category [
	"Calling this method is now considered an accident.  If you really want to create a class with a nil superclass, then create the class and then set the superclass using #superclass:"

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv subclass: `@name instanceVariableNames: `@instVar classVariableNames: `@classVar poolDictionaries: `@pools category: `@package'
			-> 'Warning signal: ''Attempt to create '' , nameOfClass , '' as a subclass of nil.  Possibly a class is being loaded before its superclass.''.
			`@rcv classInstaller
			make: [ :builder |
				builder
					superclass: nil;
					name: `@name;
					slotsFromString: `@instVar;
					sharedVariablesFromString: `@classVar;
					sharedPoolsFromString: `@pools;
					category: `@package;
					environment: `@rcv environment ]'.
	Warning signal: 'Attempt to create ' , nameOfClass , ' as a subclass of nil.  Possibly a class is being loaded before its superclass.'.
	^ self classInstaller make: [ :builder |
		  builder
			  superclass: nil;
			  name: nameOfClass;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  sharedPoolsFromString: poolDictnames;
			  category: category;
			  environment: self environment ]
]
