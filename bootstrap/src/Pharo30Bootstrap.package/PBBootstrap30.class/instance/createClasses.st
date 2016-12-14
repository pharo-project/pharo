running
createClasses
	| definitions total smalltalkInstance |
	definitions := systemDefinition allClassesAndTraits sorted: [ :a :b | a name < b name ].
	total := definitions size.
	definitions
		withIndexDo: [ :aClassDefinition :index | | time |
			time := [self createBehaviorFromDefinition: aClassDefinition.] timeToRun.
			
			self log: ((index printPaddedWith: $0 to: 3), '/' , 
								total asString, ' - ', 
									time asString ,': Built behavior ', 
										aClassDefinition name). ].
	
	smalltalkInstance := bootstrapEnvironment at: #Smalltalk.
	objectSpace interpreter: bootstrapInterpreter.
	objectSpace backend smalltalkInstance: smalltalkInstance.
	objectSpace environment: ((EPSystemDictionary withSystemDictionary: objectSpace backend systemDictionary) objectSpace: objectSpace; yourself).
	self checkpoint: 'created_classes'.