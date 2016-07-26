public api
initializeCommandLineHandlerAndErrorHandling
	"This method is run before the new bootstrapped image is saved!"
	ProcessorScheduler initialize.
	
	Delay initialize.
	SmallInteger initialize.
	
	ProcessorScheduler startUp.
	
	OSPlatform startUp: true.
	
	"2 following lines are used to initialize Unicode tag constants.
	Must be replaced by an uptodate version of Unicode class>>#initializeTagConstants and a call to it"
	#(#Cc #Cf #Co #Cs #Ll #Lm #Lo #Lt #Lu #Mc #Me #Mn #Nd #Nl #No #Pc #Pd #Pe #Pf #Pi #Po #Ps #Sc #Sk #Sm #So #Zl #Zp #Zs)
			doWithIndex: [ :each :index | Unicode classPool at: each put: index ].
Unicode classPool at: #Cn put: 0.
	UnicodeDefinitions initializeUnicodeClass.
	EncodedCharSet initialize.
	String initialize.
	ByteString initialize. "needed by TextConverter to install LineEnd convention (called by Smalltalk openLog)"
	ZnUTF8Encoder initialize.	
	
	"Weak array class initialization 2 lines"
	Smalltalk specialObjectsArray at: 42 put: Semaphore new."to put in EPObjectSpace>>#createSpecialObjectsArray?"
	WeakArray restartFinalizationProcess.
	

	DiskStore useFilePlugin "initialize".
	FileStream initialize.
	FileStream classVarNamed: 'TheStdioHandles' put: FileStream stdioHandles. "FileStream startup"
	ByteTextConverter initialize.
	MultiByteFileStream initialize.
	ASTCache default: ASTCache new. "ASTCache initialize, neeeded to print stack on error"
	FileLocator startUp: true. "FileLocator initialize"

	Smalltalk globals 
		at: #Transcript
		put: (NonInteractiveTranscript stdout install).
	CompilationContext initialize.
	Float initialize.
	ZipConstants initialize.
	ZipWriteStream initialize.

	"Initialize classes related to compiler"
	InflateStream initialize.
	ByteTextConverter initialize.
	ZipWriteStream initialize.
	SetElement initialize.
	ExternalSemaphoreTable initialize.
	MacRomanTextConverter initialize.
	Collection initialize.
	ISOLanguageDefinition initialize.
	CompiledMethod initialize.
	MD5NonPrimitive initialize.
	DangerousClassNotifier initialize.
	Categorizer initialize.
	Slot initialize.
	CP1252TextConverter initialize.
	SHA1 initialize.
	TraitBehavior initialize.
	RPackage initialize.
	CompilationContext initialize.
	OCASTTranslator initialize.

	UIManager classVarNamed: 'Default' put: NonInteractiveUIManager new.
	UIManager default activate.

	Smalltalk sourceFileVersionString: 'PharoV50'.
	(Smalltalk class classVariableNamed: 'LastImagePath') value: Smalltalk imagePath. "set the default value"
	SourceFiles := SourceFileArray new ensureOpen. "needed to get source from a compiled method, used to log an error."
	Smalltalk openSourceFiles.
	
	BasicCommandLineHandler new activate. "BasicCommandLineHandler startUp:"
	UIManager default inform: 'Alive!'.
	Processor terminateActive.