subclass creation - variableWord
variableWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s package: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable word-sized nonpointer variables."
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: WordLayout;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment;
			copyClassSlotsFromExistingClass ].