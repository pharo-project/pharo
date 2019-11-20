"
I'm the process performing the bootstrap of a Pharo 5.0 version.

	PBBootstrap50 bootstrap: '5.392' 
	
"
Class {
	#name : #PBImageBuilder50,
	#superclass : #Object,
	#instVars : [
		'objectSpace',
		'classLoader',
		'systemDefinition',
		'bootstrapInterpreter',
		'bootstrapEnvironment',
		'espellBackend',
		'statistics',
		'logger',
		'imageFileReference',
		'versionInfo'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #visiting }
PBImageBuilder50 >> behaviorFrom: aRFiClassOrTrait [ 
	^ aRFiClassOrTrait accept: self
]

{ #category : #'accessing instances and variables' }
PBImageBuilder50 >> bindingOf: aName [

	(self includesClassNamed: aName)
		ifTrue: [ ^ EPGlobalBinding new
				name: aName objectSpace: self;
				yourself ].
			
	^ nil
]

{ #category : #running }
PBImageBuilder50 >> bootstrap [

	self
		initializeBootstrapEnvironment;
		createVMStubs;
		flushNewSpace;
		createInitialObjects;
		createClasses;
		installMethods;
		installExtensionMethods;
		initializeImage
]

{ #category : #accessing }
PBImageBuilder50 >> bootstrapEnvironment [
	
	^ bootstrapEnvironment
]

{ #category : #running }
PBImageBuilder50 >> bootstrapInitialProcess [
	Smalltalk snapshotPrimitive.
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
	(Smalltalk globals at: #UnicodeDefinitions) initializeUnicodeClass.
	EncodedCharSet initialize.
	String initialize.
	ByteString initialize. "needed by TextConverter to install LineEnd convention (called by Smalltalk openLog)"
	
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

	UIManager classVarNamed: 'Default' put: NonInteractiveUIManager new.
	UIManager default activate.

	Smalltalk sourceFileVersionString: 'PharoV40'.
	(Smalltalk class classVariableNamed: 'LastImagePath') value: Smalltalk imagePath. "set the default value"
	SourceFiles := SourceFileArray new ensureOpen. "needed to get source from a compiled method, used to log an error."
	Smalltalk openSourceFiles.
	
	BasicCommandLineHandler new activate. "BasicCommandLineHandler startUp:"
	UIManager default inform: 'Alive!'.
	Processor terminateActive.
]

{ #category : #running }
PBImageBuilder50 >> bootstrapInitialProcessCode [
	| instructions |
	instructions := (self class>>#bootstrapInitialProcess) sourceCode.
	^ instructions allButFirst: (instructions indexOf: Character cr) "remove method name"
]

{ #category : #accessing }
PBImageBuilder50 >> bootstrapInterpreter [
	
	^ bootstrapInterpreter
]

{ #category : #running }
PBImageBuilder50 >> bootstrapWithResumableDebugger [
	"If we do not fork the process, we get a post-mortem debugger"
	[ self bootstrap ]
		on: Error, Halt
		fork: [ :ex | 
				Smalltalk logError: ex description inContext: nil.
				ex pass ]
]

{ #category : #accessing }
PBImageBuilder50 >> buildNumber: aNumber [

	buildNumber := aNumber
]

{ #category : #'class-loader' }
PBImageBuilder50 >> builtClassNames [
	
	^ classLoader loadedClassNames
]

{ #category : #accessing }
PBImageBuilder50 >> classForClassMirror: anEPClassMirror [

	^ classLoader classForClassMirror: anEPClassMirror
]

{ #category : #running }
PBImageBuilder50 >> classInstanceVariableIndexAt: anIndex [
	"number of instances variables of the class Class. Classes that have more than this number in their fixed size have their own instance class variable that we should probably take care of."
	^ (self classNamed: #Class) mirror fixedSize + anIndex
]

{ #category : #accessing }
PBImageBuilder50 >> classLoader [
	^ classLoader
]

{ #category : #'class-loader' }
PBImageBuilder50 >> classNamed: aString [ 
	
	^ (classLoader classNamed: aString)
		asRemoteClassOrTraitFromEnvironment: self
]

{ #category : #helpers }
PBImageBuilder50 >> compactClassIndexForClassNamed: aName [
	"To recompute, use the following expression in the image you want to bootstrap. For example, to boostrap pharo 50193, execute the expression in a pharo 50193 image.
	(Smalltalk specialObjectsArray at: 29)
		collect: [ :each | each ifNotNil: [ each name ] ]
	The order is really important, do not change it!
		 "
	^ #(
		#CompiledMethod
		#Slot
		#Array
		#LargeNegativeInteger
		#LargePositiveInteger
		#Float
		#Protocol
		#Association
		#Point
		#Rectangle
		#ByteString
		#BlockClosure
		#GlobalVariable
		#Context
		#ClassVariable
		#Bitmap
		#EndianDetector
		nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
	 indexOf: aName
]

{ #category : #running }
PBImageBuilder50 >> createBehaviorFromDefinition: aRFiDefinition [

	^ classLoader createBehaviorFromDefinition: aRFiDefinition
]

{ #category : #running }
PBImageBuilder50 >> createClasses [
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
]

{ #category : #running }
PBImageBuilder50 >> createFalse [

	objectSpace falseObject: (self createStubForClassNamed: #False) basicNew.
	^ objectSpace falseObject
]

{ #category : #running }
PBImageBuilder50 >> createInitialObjects [
	
	| characterTable |
	"We add the main globals  in the bootstrap environment before a system dictionary exists. These globals are needed to create a system dictionary, and will be used by the AST interpreter"
	PBStepFinishedSignal log: 'initializing globals'.
	self bootstrapEnvironment at: #Smalltalk put: objectSpace nilObject.
	self bootstrapEnvironment at: #Undeclared put: objectSpace nilObject.
	
	"We initialize the hash table sizes by hand.
	The AST interpreter is too slow to interpret HashTableSizes>>#initialize"
	PBSubStepFinishedSignal log: 'initializing hash table sizes'.
	
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
		('| superLayout |
		superLayout := EmptyLayout instance.
		Class
			instVarAt: 4
			put: (FixedLayout
					extending: superLayout 
					scope: (superLayout slotScope extend: #({1}))
					host: Class).
		
		SmallInteger
			instVarAt: 4
			put: (ImmediateLayout new).' format: {self espellBackend instanceVariableMapping listOfClassSlots}).
			
	PBStepFinishedSignal log: 'Class builder ready to create classes'.
	classLoader useClassBuilder.
	
	self checkpoint: 'created_initial_objects'.
]

{ #category : #running }
PBImageBuilder50 >> createNil [

	"Temporal nil: it is needed so the nil stub class can be initialized with it"
	objectSpace nilObject: (objectSpace objectMirrorOn: (UndefinedObject basicNew)).
	
	"We create the real nil and become it"
	objectSpace nilObject becomeInto: (self createStubForClassNamed: #UndefinedObject) basicNew.
	
	^ objectSpace nilObject
]

{ #category : #'class-loader' }
PBImageBuilder50 >> createStubForClassNamed: aString [ 
	
	^ classLoader createStubForClassNamed: aString
]

{ #category : #running }
PBImageBuilder50 >> createTrue [

	objectSpace trueObject: (self createStubForClassNamed: #True) basicNew.
	^ objectSpace trueObject
]

{ #category : #running }
PBImageBuilder50 >> createVMStubs [

	PBStepStartedSignal log: 'creating initial Objects needed by the VM'.
	
	objectSpace backend specialObjectsArray: (objectSpace createObjectWithFormat: (Array format) withSize: 59).
	self createNil.
	1 to: 59 do: [ :i |
		objectSpace backend specialObjectsArray at: i put: objectSpace nilObject ].
	
	self createFalse.
	self createTrue.
	
	"try to move nil, true and false to the new space"
	espellBackend objectMemory flushNewSpace.
	
	objectSpace backend smallIntegerClass: (self createStubForClassNamed: #SmallInteger).
	objectSpace backend characterClass: (self createStubForClassNamed: #Character).
	objectSpace backend byteSymbolClass: (self createStubForClassNamed: #ByteSymbol).
	objectSpace backend byteStringClass: (self createStubForClassNamed: #ByteString).
	objectSpace backend byteArrayClass: (self createStubForClassNamed: #ByteArray).
	objectSpace backend associationClass: (self createStubForClassNamed: #Association).
	objectSpace backend arrayClass: (self createStubForClassNamed: #Array).
	objectSpace backend symbolTableClass: (self createStubForClassNamed: #Symbol).
	objectSpace backend largeNegativeIntegerClass: (self createStubForClassNamed: #LargeNegativeInteger).
	objectSpace backend largePositiveIntegerClass: (self createStubForClassNamed: #LargePositiveInteger).
	objectSpace backend methodClass: (self createStubForClassNamed: #CompiledMethod).
	objectSpace backend floatClass: (self createStubForClassNamed: #Float).
	objectSpace backend contextClass: (self createStubForClassNamed: #Context).
	objectSpace backend processClass: (self createStubForClassNamed: #Process).
	objectSpace backend blockClass: (self createStubForClassNamed: #BlockClosure).
	objectSpace backend fullBlockClass: (self createStubForClassNamed: #FullBlockClosure).
	objectSpace backend messageClass: (self createStubForClassNamed: #Message).
	objectSpace backend semaphoreClass: (self createStubForClassNamed: #Semaphore).
	
	"Point is required in the special objects array because it is used to instantiate point objects faster.
	If we do not put it here, the VM will crash.
	Lots of tests in kernel use it."
	objectSpace backend pointClass: (self createStubForClassNamed: #Point).
]

{ #category : #accessing }
PBImageBuilder50 >> definitionNamed: aName [

	^ self systemDefinition classNamed: aName
]

{ #category : #accessing }
PBImageBuilder50 >> environment [
	
	^ bootstrapEnvironment 
]

{ #category : #accessing }
PBImageBuilder50 >> espellBackend [
	^ espellBackend 
]

{ #category : #accessing }
PBImageBuilder50 >> espellBackend: anObject [
	espellBackend := anObject
]

{ #category : #format }
PBImageBuilder50 >> formatOfClass: aRFiClass [ 
	
	"A class format is composed by"
	"<2 bits of size><5 bits of compact class index><4 bits of inst spec><6 bits of size><1 bit with a 0>"
	
	| size1 instSpec size2 compactClassIndex |
	size1 := (aRFiClass allInstVarNames size + 1 // 64) bitAnd: 16r3.
	instSpec := aRFiClass isCompiledMethod
					ifTrue: [ 12 ]
					ifFalse: [aRFiClass isWeak
						ifTrue:[ 4 ]
						ifFalse:[aRFiClass isPointers
									ifTrue: [ aRFiClass isVariable
												ifTrue: [ aRFiClass allInstVarNames size > 0 ifTrue: [ 3 ] ifFalse: [ 2 ] ]
												ifFalse: [ aRFiClass allInstVarNames size > 0 ifTrue: [ 1 ] ifFalse: [ 0 ] ]
											]
									ifFalse: [ aRFiClass isWords ifTrue: [ 6 ] ifFalse: [ 8 ] ]
								]
					].
	size2 := (aRFiClass allInstVarNames size + 1 \\ 64) bitAnd: 16r3F.
	
	compactClassIndex := self compactClassIndexForClass: aRFiClass.
	
	^(size1 bitShift: 16) + (compactClassIndex bitShift: 11) + (instSpec bitShift: 7) + (size2 bitShift: 1)
]

{ #category : #accessing }
PBImageBuilder50 >> globalNamed: aString [ 
	
	(self includesClassNamed: aString)
		ifTrue: [ ^ (self classNamed: aString) mirror ].
	self halt.
]

{ #category : #accessing }
PBImageBuilder50 >> globalNamed: aString put: aValue [
	
	bootstrapEnvironment at: aString put: aValue
]

{ #category : #accessing }
PBImageBuilder50 >> imageFileReference: aFileReference [ 
	
	imageFileReference := aFileReference
]

{ #category : #testing }
PBImageBuilder50 >> includesClassNamed: aString [
	
	^ self systemDefinition includesClassNamed: aString
]

{ #category : #testing }
PBImageBuilder50 >> includesGlobalNamed: aString [ 
	
	"It is a global if we have it in our system definition"
	^ self systemDefinition includesClassNamed: aString.
]

{ #category : #'initialize-release' }
PBImageBuilder50 >> initialize [

	super initialize.
	statistics := PBBootstrapStatistics new.
	logger := PBBootstrapLogger new.
	imageFileReference := 'bootstrap.image' asFileReference.

	"the default espell backend for the bootstrap"
	self initializeBackend.
	self instantiateClassLoader.
]

{ #category : #'initialize-release' }
PBImageBuilder50 >> initializeBackend [

	self subclassResponsibility
]

{ #category : #running }
PBImageBuilder50 >> initializeBootstrapEnvironment [
	
	bootstrapEnvironment := Dictionary new.
	
	"We initialize the object space with a boot symbol table and character table.
	We will later replace them by real ones."
	objectSpace := EPObjectSpace new.
	objectSpace backend: espellBackend.
	objectSpace characterTable: (EPExternalCharacterTable new objectSpace: objectSpace; yourself).
	objectSpace symbolTable: (EPExternalSymbolTable new objectSpace: objectSpace; yourself).
	
	classLoader createJustStubs.
	classLoader environment: bootstrapEnvironment.
	classLoader systemDefinition: self systemDefinition.
	classLoader builder: self.
	
	bootstrapInterpreter := EPASTEvaluator new.
	bootstrapInterpreter codeProvider: self.
	bootstrapInterpreter environment: self.
	bootstrapInterpreter objectSpace: objectSpace.
	objectSpace interpreter: bootstrapInterpreter.
]

{ #category : #running }
PBImageBuilder50 >> initializeImage [
	| process |

	self flag: 'should migrate this method'.
	self checkpoint: 'image_initialization'.
	
	self initializeSpecialObjectArray.
	
	self bootstrapInterpreter evaluateCode: '
	Smalltalk at: #Processor put: (ProcessorScheduler basicNew).
	(Smalltalk at: #Processor) instVarAt: 1 put: ((1 to: 80) collect: [ :i | ProcessList new ])'.

	"Initialize the processor association for the special objects array.
	Enchancement possible: This should be automatically managed by the createVMStubs method"
	objectSpace backend processorAssociation: (self bootstrapInterpreter evaluateCode: 'Smalltalk globals associationAt: #Processor.').
	
	process := objectSpace
		createProcessWithPriority: 40 
		doing: ('PharoBootstrapInitialization 
			initializeImageOfType: ''Pharo'' 
			major: {major} 
			minor: {minor} 
			patch: {patch}
			suffix: ''{suffix}'' 
			build: {build} 
			commitHash: ''{hash}''.' 
		format: {
			'major' -> versionInfo major.
			'minor' -> versionInfo minor.
			'patch' -> versionInfo patch.
			'suffix' -> (versionInfo suffix ifNil: [ '' ]).
			'build' -> versionInfo buildNumber.
			'hash' -> versionInfo commitHash.
	} asDictionary).
 	objectSpace installAsActiveProcess: process.
	
	self followForwardingPointers.

"	espellBackend simulator stackPages pages do: [ :page | espellBackend simulator stackPages freeStackPage: page ].
	espellBackend simulator freeUntracedStackPages."
	[[ espellBackend objectMemory flushNewSpace. ]
		on: Halt
		do: [:ex|
			"suppress halts from the usual suspects (development time halts)"	
			(#(#fullGC #compactImage  #doScavenge:) includes: ex signalerContext sender selector)
				ifTrue: [ex resume]
				ifFalse: [ex pass]]] on: AssertionFailure do: [ :e |
				((e signalerContext sender method selector == #assert:)
					and: [ e signalerContext sender sender method selector = #mapStackPages ])
						ifTrue: [ e resume ]
						ifFalse: [ e pass ]] .

	self freeForwarders.
	self compactImage.
	self reportSizes.

	self writeSnapshot: imageFileReference fullName ofTransformedImage: espellBackend simulator objectMemory headerFlags: 0 screenSize: 0.
		PBStepFinishedSignal log: 'Installed extension methods'.

]

{ #category : #running }
PBImageBuilder50 >> initializeSpecialObjectArray [
"	objectSpace mustRebuildSpecialObjectsArray.
"	objectSpace specialObjectsArray: self newSpecialObjectsArray.

	self bootstrapInterpreter
		evaluateCode: 'Smalltalk specialObjectsArray: newArray.'
		withTemps: { 'newArray' -> objectSpace backend specialObjectsArray }.
]

{ #category : #running }
PBImageBuilder50 >> installExtensionMethods [
	
	| definitions total |
	definitions := self systemDefinition packages sorted: [ :a :b | a name < b name ].
	total := definitions size.
	definitions withIndexDo: [ :aPackageDefinition :index |
		self installExtensionMethodsOf: aPackageDefinition prefixed: (index printPaddedWith: $0 to: 3), '/' , total asString.
	].

	PBStepFinishedSignal log: 'Installed extension methods'.
	self checkpoint: 'installed_extension_methods'.
]

{ #category : #running }
PBImageBuilder50 >> installExtensionMethodsOf: aPackageDefinition prefixed: aPrefix [
	aPackageDefinition extensionMethods
		doWithIndex: [ :methodDefinition :index | | compiledMethod behaviorMirror methodClass logMessage |
			methodClass :=  methodDefinition  methodClass.
			compiledMethod := self methodBuilder createNewMethod: methodDefinition.
			behaviorMirror := bootstrapEnvironment at:  methodClass theNonMetaClass name.
			methodClass isMeta ifTrue: [ behaviorMirror := behaviorMirror classSide ].
			
		logMessage := String streamContents: [ :stream |
			stream
				nextPutAll: aPrefix;
				nextPut: $-;
				nextPutAll: index asString;
				nextPut: $/;
				nextPutAll: aPackageDefinition extensionMethods size asString;
				nextPutAll: ': Installing extension method ';
				nextPutAll: methodClass name;
				nextPutAll: '>>';
				nextPutAll: methodDefinition selector ].
		PBSubStepFinishedSignal log: logMessage.
			
		self installMethod: compiledMethod inBehavior: behaviorMirror ]
]

{ #category : #running }
PBImageBuilder50 >> installMethod: method inBehavior: theClass [

	| timePerMethod |
	timePerMethod := [self objectSpace interpreter
		evaluateCode: 'theBehavior addSelectorSilently: selector withMethod: compiledMethod'
		withTemps: { 
			'theBehavior' -> theClass.
			'selector' -> (method selector asLiteralInObjectSpace: objectSpace).
			'compiledMethod' -> method }] timeToRun.
	
	statistics addMethodTime: timePerMethod.
]

{ #category : #running }
PBImageBuilder50 >> installMethods [
	
	| definitions total |
	definitions := self systemDefinition allClassesAndTraits sorted: [ :a :b | a name < b name ].
	total := definitions size.
	definitions withIndexDo: [ :aClassDefinition :index |
		"self checkpoint: 'installed_methods_wip'."
		self installMethodsInBehaviorDefinition: aClassDefinition prefixed: (index printPaddedWith: $0 to: 3), '/' , total asString.
	].
	PBStepFinishedSignal log: 'installed_methods'.
	self checkpoint: 'installed_methods'.
]

{ #category : #running }
PBImageBuilder50 >> installMethodsInBehaviorDefinition: behaviorDefinition prefixed: aPrefix [
	| behaviorMirror  |
	behaviorMirror := bootstrapEnvironment at: behaviorDefinition name.
	self
		installMethodsInBehaviorMapping: behaviorDefinition
		using: self methodBuilder
		onMirror: behaviorMirror
		prefixed: aPrefix.
	self
		installMethodsInBehaviorMapping: behaviorDefinition classSide
		using: self methodBuilder
		onMirror: behaviorMirror basicClass
		prefixed: aPrefix.
]

{ #category : #running }
PBImageBuilder50 >> installMethodsInBehaviorMapping: aBehaviorMapping using: aMethodBuilder onMirror: mirror prefixed: aPrefix [
	| newMethods theClass |
	newMethods := (aMethodBuilder methodsFromDefinition: aBehaviorMapping) sorted: [ :a :b | a selector < b selector ].
	theClass := (self classNamed: aBehaviorMapping theNonMetaClass name) mirror.
	aBehaviorMapping isMeta ifTrue: [ theClass := theClass classSide ].
	newMethods doWithIndex: [ :method :index |
		PBSubStepFinishedSignal log: aPrefix, ' - ', index asString, '/', newMethods size asString, ': Installing method ', 
			aBehaviorMapping name, '>>', method selector.
		
		self installMethod: method inBehavior: theClass.
	].
]

{ #category : #'initialize-release' }
PBImageBuilder50 >> instantiateClassLoader [

	classLoader := PBClassLoader new.
]

{ #category : #testing }
PBImageBuilder50 >> isMeta: aMirror [
	
	^ (self classNamed: #Metaclass) mirror = aMirror basicClass
]

{ #category : #helpers }
PBImageBuilder50 >> log: aLogStatement [

	logger log: aLogStatement
]

{ #category : #accessing }
PBImageBuilder50 >> metaclassForClassMirror: anEPClassMirror [

	^ EPRemoteMetaClass on: anEPClassMirror environment: self
]

{ #category : #accessing }
PBImageBuilder50 >> methodBuilder [
	^ PBMethodCompiler new builder: self; yourself.	
]

{ #category : #accessing }
PBImageBuilder50 >> objectSpace [
	^ objectSpace
]

{ #category : #accessing }
PBImageBuilder50 >> statistics [

	^ statistics
]

{ #category : #definition }
PBImageBuilder50 >> systemDefinition [

	^ systemDefinition
]

{ #category : #accessing }
PBImageBuilder50 >> systemDefinition: anObject [
	systemDefinition := anObject
]

{ #category : #accessing }
PBImageBuilder50 >> versionInfo [
	^ versionInfo
]

{ #category : #accessing }
PBImageBuilder50 >> versionInfo: anObject [
	versionInfo := anObject
]

{ #category : #visiting }
PBImageBuilder50 >> visitClass: aRFiClass [
	^ EPRemoteClass 
		on: (classLoader classNamed: aRFiClass name)
		environment: self
]

{ #category : #visiting }
PBImageBuilder50 >> visitTrait: aRFiTrait [ 
	^ EPRemoteTrait 
		on: (classLoader classNamed: aRFiTrait name)
		environment: self
]
