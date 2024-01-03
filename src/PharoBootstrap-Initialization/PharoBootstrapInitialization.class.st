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
	#name : 'PharoBootstrapInitialization',
	#superclass : 'Object',
	#category : 'PharoBootstrap-Initialization-Base',
	#package : 'PharoBootstrap-Initialization',
	#tag : 'Base'
}

{ #category : 'public api' }
PharoBootstrapInitialization class >> initializeASTDeprecations [
	"This should be removed in P13 with all the deprecated aliases setted."
	{
		ASTLiteralValueNode.
		ASTMethodNode.
		ASTCascadeNode.
		ASTStringReplacement.
		ASTArrayNode.
		ASTNode.
		ASTVariableNode.
		ASTAnnotationMarkNode.
		ASTLiteralArrayNode.
		ASTReturnNode.
		ASTLiteralNode.
		ASTSelectorNode.
		ASTBlockNode.
		ASTComment.
		ASTAssignmentNode.
		ASTPragmaNode.
		ASTValueNode.
		ASTMessageNode.
		ASTProgramNode.
		ASTDoItMethodNode.
		ASTSequenceNode } do: [ :class | class initialize ]
]

{ #category : 'initialization' }
PharoBootstrapInitialization class >> initializeCommandLineHandlerAndErrorHandling [
	"This method is run before the new bootstrapped image is saved!"
	
	ProcessorScheduler initialize.

	Delay initialize.

	ProcessorScheduler startUp.

	OSPlatform startUp: true.

	SmallInteger initialize.
	String initialize.
	ByteString initialize. 
	Float initialize.
	
	ZnUTF8Encoder initialize. "needed by TextConverter to install LineEnd convention (called by Smalltalk openLog)"

	"Weak array class initialization 2 lines"
	Smalltalk specialObjectsArray at: 42 put: Semaphore new."to put in EPObjectSpace>>#createSpecialObjectsArray?"
	FinalizationProcess restartFinalizationProcess.

	Smalltalk globals
		at: #Transcript
		put: (NonInteractiveTranscript stdout install).


	UIManager default: NonInteractiveUIManager new.
	ErrorHandler defaultErrorHandler: UIManager default.

	InstructionStream initialize.

	'Initializing Collections' traceCr.
	CollectionElement initialize.
	ExternalSemaphoreTable initialize.
	
	'Initializing code model' traceCr.
	CompiledMethod initialize.
	Slot initialize.
	Behavior initialize.
	Class initialize.
	Package initialize.
	"Next three lines are for the deprecated aliases. This should be removed in Pharo 13"
	PackageTag initialize.
	PackageOrganizer initialize.
	PackageConflictError initialize.

	'Initializing sources' traceCr.
	Smalltalk sourceFileVersionString: 'PharoV60'.
	(Smalltalk class classVariableNamed: 'LastImagePath') value: Smalltalk imagePath. "set the default value"
	SourceFiles := SourceFileArray new.

	'InitializingFFI' traceCr.
	"FFI"
	ExternalObject initialize.
	ExternalType initialize.

	'Initializing Session Manager' traceCr.
	SessionManager initialize.
	SessionManager initializeKernelRegistrations.
	SessionManager default installNewSession.


	'Initializing basic command line behaviors' traceCr.
	"Initialize basic command line behaviour"
	NonInteractiveTranscript initialize.
	BasicCommandLineHandler initialize.

	Smalltalk snapshot: true andQuit: true.
	Processor terminateActive
]

{ #category : 'public api' }
PharoBootstrapInitialization class >> initializeFileSystem [

	| env |
	
	'Initializing File System' traceCr.
	env := self environment.
	(env at: #FileLocator) initialize.
	(env at: #DiskStore) initialize.
	(env at: #File) initialize.
	(env at: #SharedRandom) initialize.

	"Now that the file system is setup, we can register SourceFileArray to the session manager."
	SessionManager default registerSystemClassNamed: #SourceFileArray
]
