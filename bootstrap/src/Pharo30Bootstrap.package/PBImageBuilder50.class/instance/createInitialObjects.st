running
createInitialObjects
	
	| characterTable |
	"We add the main globals  in the bootstrap environment before a system dictionary exists. These globals are needed to create a system dictionary, and will be used by the AST interpreter"
	PBStepFinishedSignal log: 'initializing globals'.
	self bootstrapEnvironment at: #Smalltalk put: objectSpace nilObject.
	self bootstrapEnvironment at: #Undeclared put: objectSpace nilObject.
	
	"We initialize the hash table sizes by hand.
	The AST interpreter is too slow to interpret HashTableSizes>>#initialize"
	PBSubStepFinishedSignal log: 'initializing hash table sizes'.
	self flag: 'check if it works faster with the optimized interpreter'.
	(self classNamed: #HashTableSizes) mirror
		instanceVariableAtIndex: (self classInstanceVariableIndexAt: 1)
		put: (objectSpace newArrayWithAll: (HashTableSizes sizes collect: [:each | objectSpace backend smallIntegerMirrorOn: each ])).
	
	PBSubStepFinishedSignal log: 'preparing class builder'.
	self bootstrapInterpreter evaluateCode: 'DangerousClassNotifier disable'.

	self bootstrapInterpreter evaluateCode: 'Undeclared := Dictionary new.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk := SmalltalkImage basicNew.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk instVarAt: 1 put: (SystemDictionary withOrganizer: SystemOrganizer new).'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk globals at: #Smalltalk put: Smalltalk.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk globals at: #Undeclared put: Undeclared.'.

	"Initialize the globals of the system. Careful: The AST interpreter will not know about these if we do not put them in the bootstrapEnvironment."	
	self bootstrapInterpreter evaluateCode: 'Smalltalk globals at: #Processor put: nil.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk globals at: #Transcript put: nil.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk globals at: #SystemOrganisation put: nil.'.
	self bootstrapInterpreter evaluateCode: 'Smalltalk globals at: #SourceFiles put: nil.'.

	
	PBSubStepFinishedSignal log: 'class loader now creates class pools'.
	classLoader initializeClassPools.

	PBSubStepFinishedSignal log: 'Initializing StartUp list'.
	self bootstrapInterpreter evaluateCode: 'SmalltalkImage initialize.'.
	
	PBSubStepFinishedSignal log: 'Initializing Character Table'.
	self bootstrapInterpreter evaluateCode: 'Character initialize.'.
	characterTable := (EPInternalCharacterTable new objectSpace: objectSpace).
	objectSpace backend characterTable: ((self classNamed: #Character) classPool at: #CharacterTable).
	objectSpace characterTable: characterTable.
	
	PBSubStepFinishedSignal log: 'Initializing String AsciiTable'.
	self bootstrapInterpreter evaluateCode: 'String classPool at: #AsciiOrder put: ((0 to: 255) as: ByteArray).'.
	
	PBSubStepFinishedSignal log: 'Initializing SmallInteger constants'.
	self bootstrapInterpreter evaluateCode: 'SmallInteger instVarAt: ' , (self classInstanceVariableIndexAt: 1) asString ,' put: -16r40000000.' "minVal".
	self bootstrapInterpreter evaluateCode: 'SmallInteger instVarAt: ' , (self classInstanceVariableIndexAt: 2) asString ,' put: 16r3FFFFFFF.' "maxVal".

	PBSubStepFinishedSignal log: 'Initializing Date and Time constants'.
	self bootstrapInterpreter evaluateCode: 'DateAndTime initialize.'.
	self bootstrapInterpreter evaluateCode: 'ChronologyConstants initialize.'.
	
	PBSubStepFinishedSignal log: 'Initializing CompiledMethod constants'.
	self bootstrapInterpreter evaluateCode: 'CompiledMethod initialize.'.
	
	PBSubStepFinishedSignal log: 'Initializing object dependent fields'.
	self bootstrapInterpreter evaluateCode: 'Object initialize.'.
	
	PBSubStepFinishedSignal log: 'Installing symbol table'.
	self bootstrapInterpreter evaluateCode: 'Symbol initialize.
	Symbol classPool at: #SymbolTable put: (WeakSet withAll: ByteSymbol allInstances)'.
	objectSpace symbolTable: (EPInternalSymbolTable new objectSpace: objectSpace).

	
	PBSubStepFinishedSignal log: 'Bootstrapping class layouts'.
	bootstrapInterpreter evaluateCode:
		'| superLayout |
		superLayout := EmptyLayout instance.
		Class
			instVarAt: 4
			put: (FixedLayout
					extending: superLayout 
					scope: (superLayout slotScope extend: #(superclass methodDict format layout instanceVariables organization subclasses name classPool sharedPools environment category traitComposition localSelectors))
					host: Class).
		Class
			instVarAt: 5 put: #(superclass methodDict format layout instanceVariables organization subclasses name classPool sharedPools environment category traitComposition localSelectors).
			
		SmallInteger
			instVarAt: 4
			put: (ImmediateLayout new).'.
			
	PBStepFinishedSignal log: 'Class builder ready to create classes'.
	classLoader useClassBuilder.
	
	self checkpoint: 'created_initial_objects'.