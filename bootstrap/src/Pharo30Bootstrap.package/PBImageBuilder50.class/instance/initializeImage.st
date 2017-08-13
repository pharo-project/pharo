running
initializeImage
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
		"doing: self bootstrapInitialProcessCode."
		doing: ('
	SystemVersion 
		writeClassVariableNamed: #Current
		value: (SystemVersion new
					type: ''Pharo'';
					major: {major};
					minor: {minor};
					suffix: ''SNAPSHOT'';
					commitHash: ''{update}'';
					yourself).
	PharoBootstrapInitialization initializeCommandLineHandlerAndErrorHandling' format: {
		'major' -> definitionFetcher majorVersion asString.
		'minor' -> definitionFetcher minorVersion asString.
		'update' -> definitionFetcher commitHash asString.
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
				((e signalerContext method selector == #assert:)
					and: [ e signalerContext sender method selector = #mapStackPages ])
						ifTrue: [ e resume ]
						ifFalse: [ e pass ]] .

	self freeForwarders.
	self compactImage.
	self reportSizes.

	self writeSnapshot: imageFileReference fullName ofTransformedImage: espellBackend simulator objectMemory headerFlags: 0 screenSize: 0.
		PBStepFinishedSignal log: 'Installed extension methods'.