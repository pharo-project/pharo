subclass creation - variableByte
variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable byte-sized nonpointer variables."
	
	| oldClassOrNil actualLayoutClass |
	oldClassOrNil := self environment at: t ifAbsent:[nil].
	actualLayoutClass := (oldClassOrNil notNil
				   and: [oldClassOrNil classLayout class  == CompiledMethodLayout ])
					ifTrue: [CompiledMethodLayout]
					ifFalse: [ByteLayout].
					
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: actualLayoutClass;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self  environment;
			copyClassSlotsFromExistingClass ].