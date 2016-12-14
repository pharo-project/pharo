running
createClasses
	| definitions total smalltalkInstance |
	definitions := self systemDefinition allClassesAndTraits sorted: [ :a :b | a name < b name ].
	total := definitions size.
	definitions
		withIndexDo: [ :aClassDefinition :index | | time |
			time := [self createBehaviorFromDefinition: aClassDefinition.] timeToRun.
			
			PBSubStepFinishedSignal log: ((index printPaddedWith: $0 to: 3), '/' , 
								total asString, ' - ', 
									time asString ,': Built behavior ', 
										aClassDefinition name). ].
	
	smalltalkInstance := bootstrapEnvironment at: #Smalltalk.
	objectSpace interpreter: bootstrapInterpreter.
	objectSpace backend smalltalkInstance: smalltalkInstance.
	objectSpace environment: ((EPSystemDictionary withSystemDictionary: objectSpace backend systemDictionary) objectSpace: objectSpace; yourself).
	
	PBStepFinishedSignal log: 'Classes created'.
	self checkpoint: 'created_classes'.