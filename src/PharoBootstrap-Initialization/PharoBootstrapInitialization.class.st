"
I hold the code needed to initialize correctly a fresh generated bootstrap image.

I am invoked from the only process in the bootstraped image. 

This process sends the message

initializeImageOfType: typeName majorVersion: major minor: minor suffix: suffix buid: build commitHash: hash

- The typeName is 'Pharo'
- Major and Minor are the versions of Pharo
- Suffix is the suffix of the version.
- Build is the build number from the CI server.
- hash is the commitish that produced this image.

I initialize the version object of the Image and perform the initial initialization.
Check #initializeCommandLineHandlerAndErrorHandling to see the steps performed.
"
Class {
	#name : #PharoBootstrapInitialization,
	#superclass : #Object,
	#category : #'PharoBootstrap-Initialization-Base'
}

{ #category : #initialization }
PharoBootstrapInitialization class >> initializeCommandLineHandlerAndErrorHandling [
	"This method is run before the new bootstrapped image is saved!"
	ProcessorScheduler initialize.
	
	Object initialize.
	
	ChronologyConstants initialize.
	DateAndTime initialize.
	SessionManager default installNewSession.
	
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
	Float initialize.

	CollectionElement initialize.
	ExternalSemaphoreTable initialize.
	Collection initialize.
	CompiledMethod initialize.
	Categorizer initialize.
	Slot initialize.
	RPackage initialize.
	Behavior initialize.

	UIManager classVarNamed: 'Default' put: NonInteractiveUIManager new.
	UIManager default activate.

	Smalltalk sourceFileVersionString: 'PharoV60'.
	(Smalltalk class classVariableNamed: 'LastImagePath') value: Smalltalk imagePath. "set the default value"
	SourceFiles := SourceFileArray new.
		
	Color initialize.
	InstructionStream initialize.
	SmallInteger initialize.

	"FFI"
	ExternalObject initialize.
	ExternalFunction initialize.
	ExternalType initialize.

	"Initialize basic command line behaviour"
	Stdio initialize.
	NonInteractiveTranscript initialize.
	BasicCommandLineHandler initialize.
	Smalltalk snapshot: true andQuit: true.
	Processor terminateActive.

]

{ #category : #'public api' }
PharoBootstrapInitialization class >> initializeFileSystem [
	| env |
	env := self environment.
	(env at: #FileLocator) initialize.
	(env at: #DiskStore) initialize.
	(env at: #FileHandle) initialize.
	(env at: #MultiByteFileStream) initialize.
	(env at: #File) initialize
]

{ #category : #'public api' }
PharoBootstrapInitialization class >> initializeImageOfType: typeName major: major minor: minor patch: patch suffix: suffix build: buildNumber commitHash: hash [

	"The image main entry point called by an image builder. The arguments specify the image
	version description.
	This method is run before the new bootstrapped image is saved!"

	SystemVersion 
		writeClassVariableNamed: #Current
		value: (SystemVersion new
					type: typeName;
					major: major;
					minor: minor;
					patch: patch;
					suffix: suffix;
					build: buildNumber;
					commitHash: hash;
					yourself).
	
	self initializeCommandLineHandlerAndErrorHandling.
]
