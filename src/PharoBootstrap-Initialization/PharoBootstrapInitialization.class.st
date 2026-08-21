"
I hold the code needed to initialize correctly a fresh generated bootstrap image.

I am invoked from the only process in the bootstraped image. 

I have multiple usage.

## Basic image initialization

I initialize the version object of the Image and perform the initial initialization.
Check #initializeCommandLineHandlerAndErrorHandling to see the steps performed.

## Fix methods

I fix some bugs with the protocols of methods and extension methods.

Protocols are provided in file where every method has one line and provides following data separated by  tabs:

•	class name
•	method is on the class side (true/false)
•	method selector
•	protocol name


The <cr> line ending is expected for protocols.

Protocols file example:
```
ASTReplaceRule	false	searchForTree:replaceWith:	matching
ASTPatternPragmaNode	true	selector:keywordsPositions:arguments:	instance creation
```

Examples:
```language=bash
pharo Pharo.image initializePackages --protocols=protocols.txt --save
```

## Enabling of the change log

I enable the change log once Metacello is loaded and before we reload the full code of the image.
"
Class {
	#name : 'PharoBootstrapInitialization',
	#superclass : 'Object',
	#category : 'PharoBootstrap-Initialization-Base',
	#package : 'PharoBootstrap-Initialization',
	#tag : 'Base'
}

{ #category : 'fixing' }
PharoBootstrapInitialization class >> fixExtensionMethods [

	| allBehaviors |
	allBehaviors := Smalltalk allClassesAndTraits.

	"This is a hack because of a weird behavior produced by the bootstrap were the package or tags end up containing classes that are not the same instances than we have in the system dictionary..To be investigated..."
	PackageOrganizer default packages do: [ :package |
		package tags do: [ :tag |
			tag instVarNamed: #classes put: (tag classes collect: [ :class | allBehaviors detect: [ :aClass | aClass name = class name ] ]) ] ].

	self flag: #todo. "This next step should not be needed when we will not use announcements to manage method addition"
	allBehaviors
		do: [ :behavior |
			{
				behavior.
				behavior classSide } do: [ :aBehavior |
				aBehavior extensionProtocols do: [ :protocol |
					(PackageOrganizer default ensurePackageMatching: protocol name allButFirst trimBoth) importProtocol: protocol forClass: aBehavior ] ] ]
]

{ #category : 'private' }
PharoBootstrapInitialization class >> fixMethod: methodSpecItems [

	| class selector |
	class := Smalltalk classOrTraitNamed: methodSpecItems first.
	methodSpecItems second = 'true' ifTrue: [ class := class classSide ].
	selector := methodSpecItems third asSymbol.

	class ifNil: [
			self error:
				'Could not found class: ' , methodSpecItems first printString
				, ' for selector: ' , selector printString ].

	(class selectors includes: selector) ifTrue: [
		class classify: selector under: methodSpecItems fourth ]
]

{ #category : 'fixing' }
PharoBootstrapInitialization class >> fixMethodsIn: aFileName [

	(File named: aFileName) readStreamDo: [ :binaryStream |
			| stream |
			stream := ZnCharacterReadStream on: binaryStream encoding: 'utf8'.
			[ stream atEnd ] whileFalse: [
					| line items |
					line := stream upTo: Character cr.
					items := line findTokens: String tab.
					self fixMethod: items ] ]
]

{ #category : 'initialization' }
PharoBootstrapInitialization class >> initializeCommandLineHandlerAndErrorHandling [
	"This method is run before the new bootstrapped image is saved!"
	
	ProcessorScheduler initialize.

	Delay initialize.

	ProcessorScheduler startUp.

	OSPlatform startUp: true.

	AbstractAnnouncementSubscription initialize.
	
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

	ErrorHandler default: NonInteractiveErrorHandler new.

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

	'Initializing sources' traceCr.
	SourcesManager initialize.

	'Initializing Session Manager' traceCr.
	SessionManager initialize.
	SessionManager initializeKernelRegistrations.


	'Initializing basic command line behaviors' traceCr.
	"Initialize basic command line behaviour"
	NonInteractiveTranscript initialize.
	PerformMessageCommandLineHandler registerAsCommandLinehandler.

	self undeclaredRegistry verifyEmpty.

	Smalltalk snapshot: true andQuit: true.
	Processor terminateActive
]
