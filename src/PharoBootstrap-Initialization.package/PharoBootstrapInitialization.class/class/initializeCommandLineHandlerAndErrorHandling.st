public api
initializeCommandLineHandlerAndErrorHandling
	"This method is run before the new bootstrapped image is saved!"
	ProcessorScheduler initialize.
	
	Delay initialize.
	SmallInteger initialize.
	
	ProcessorScheduler startUp.
	
	OSPlatform startUp: true.
	
	String initialize.
	ByteString initialize. "needed by TextConverter to install LineEnd convention (called by Smalltalk openLog)"
	ZnUTF8Encoder initialize.	
	
	"Weak array class initialization 2 lines"
	Smalltalk specialObjectsArray at: 42 put: Semaphore new."to put in EPObjectSpace>>#createSpecialObjectsArray?"
	WeakArray restartFinalizationProcess.
	
	Smalltalk globals 
		at: #Transcript
		put: (NonInteractiveTranscript stdout install).
	CompilationContext initialize.
	Float initialize.

	"Initialize classes related to compiler"
"	InflateStream initialize.
	ByteTextConverter initialize.
	ZipWriteStream initialize."
	SetElement initialize.
	ExternalSemaphoreTable initialize.
"	MacRomanTextConverter initialize."
	Collection initialize.
"	ISOLanguageDefinition initialize."
	CompiledMethod initialize.
"	MD5NonPrimitive initialize.
	DangerousClassNotifier initialize."
	Categorizer initialize.
	Slot initialize.
	"CP1252TextConverter initialize.
	SHA1 initialize."
	TraitBehavior initialize.
	RPackage initialize.
"	CompilationContext initialize."
"	OCASTTranslator initialize.
"
	UIManager classVarNamed: 'Default' put: NonInteractiveUIManager new.
	UIManager default activate.

	Smalltalk sourceFileVersionString: 'PharoV60'.
	(Smalltalk class classVariableNamed: 'LastImagePath') value: Smalltalk primImagePath. "set the default value"
	SourceFiles := SourceFileArray new.
	
	BasicCommandLineHandler new activate. "BasicCommandLineHandler startUp:"
	Processor terminateActive.