Extension { #name : 'Trait' }

{ #category : '*Deprecated12' }
Trait class >> named: aName [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName instanceVariableNames: aString package: aPackageName [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name instanceVariableNames: `@instVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					slotsFromString: `@instVar;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slotsFromString: aString;
			  category: aPackageName;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName package: aString [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aString;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aCompositionOrArray [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					traitComposition: `@trait;
					environment: `@rcv environment ]'.

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  traitComposition: aCompositionOrArray;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection category: aPackageName [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait category: `@package env: `@env' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					traitComposition: `@trait;
					category: `@package;
					environment: `@env ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection category: aPackageName env: anEnvironment [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait category: `@package env: `@env' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					traitComposition: `@trait;
					category: `@package;
					environment: `@env ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection instanceVariableNames: aString package: aPackageName [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait instanceVariableNames: `@instVar package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					slotsFromString: `@instVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slotsFromString: aString;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection instanceVariableNames: aString package: aPackageName env: anEnvironment [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait instanceVariableNames: `@instVar package: `@package env: `@env' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					slotsFromString: `@instVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@env ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  environment: anEnvironment;
			  slotsFromString: aString;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection package: aString [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait package: `@package' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					traitComposition: `@trait;
					package: `@package;
					environment: `@rcv environment ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aString;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection package: aString env: anEnvironment [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait package: `@package env: `@env' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					traitComposition: `@trait;
					package: `@package;
					environment: `@env ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aString;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots category: aCategory [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait slots: `@instVar category: `@package env: `@env' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					slots: `@instVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@env ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slots: someSlots;
			  category: aCategory;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots category: aCategory env: anEnvironment [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait slots: `@instVar category: `@package env: `@env' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					slots: `@instVar;
					traitComposition: `@trait;
					category: `@package;
					environment: `@env ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slots: someSlots;
			  category: aCategory;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots package: aPackageName [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait slots: `@instVar package: `@package env: `@env' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					slots: `@instVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@env ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slots: someSlots;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : '*Deprecated12' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots package: aPackageName env: anEnvironment [

	self
		deprecated: 'This method uses the old way of creating classes. The new way is to use the ShiftClassInstaller.'
		transformWith: '`@rcv named: `@name uses: `@trait slots: `@instVar package: `@package env: `@env' -> '`@rcv classInstaller
			make: [ :builder |
				builder
					beTrait;
					name: `@name;
					slots: `@instVar;
					traitComposition: `@trait;
					package: `@package;
					environment: `@env ]'.
	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slots: someSlots;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]
