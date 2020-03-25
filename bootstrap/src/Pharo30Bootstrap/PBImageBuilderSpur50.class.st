"
I implement the initial image building for Spur VMs. I'm an abstract class. My subclasses implement specifics about 32 and 64 bit images.
"
Class {
	#name : #PBImageBuilderSpur50,
	#superclass : #PBImageBuilder50,
	#instVars : [
		'nextOrdinaryClassIndex',
		'freeListsOop'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #examples }
PBImageBuilderSpur50 class >> example5617 [

	| vm om |
	vm := StackInterpreterSimulator newWithOptions: #(#ObjectMemory #Spur32BitMemoryManager ).
	om := vm objectMemory.
	vm desiredNumStackPages: 8. "Makes simulation faster by creating fewer stack pages."
	vm instVarNamed: 'assertVEPAES' put: false.
	vm initStackPages.
	PBImageBuilderSpur50 new
		version: '5.617';
		espellBackend: (EPSimulatorBackend simulator: vm)
			forBootstrap;
		bootstrap.
]

{ #category : #'instance-creation' }
PBImageBuilderSpur50 class >> forArchitecture: architecture [

	| candidates |
	candidates := self subclasses select: [ :sc | sc name endsWith: architecture, 'bit' ].
	
	candidates 
		ifEmpty: [ self error: 'No candidates for architecture: ', architecture ].
	candidates size > 1
		ifTrue: [ self error: 'More than one candidate for architecture: ', architecture ].
		
	^ candidates anyOne new
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> allocateClassTable [
	"Allocate the root of the classTable plus enough pages to accomodate all classes in
	 the classToIndex map.  Don't fill in the entries yet; the classes have yet to be cloned."
	| tableRoot page maxSize numPages newHeap newOop |
	newHeap := objectSpace backend simulator objectMemory.
	tableRoot := newHeap
					allocateSlots: newHeap classTableRootSlots + newHeap hiddenRootSlots
					format: newHeap arrayFormat
					classIndex: newHeap arrayClassIndexPun.
	newOop := tableRoot.
	self haltIf: [ newOop = 4239888 ].
	self assert: (newHeap numSlotsOf: tableRoot) = (newHeap classTableRootSlots + newHeap hiddenRootSlots).
	self assert: (newHeap formatOf: tableRoot) = newHeap arrayFormat.
	self assert: (newHeap classIndexOf: tableRoot) = newHeap arrayClassIndexPun.
	newHeap nilFieldsOf: tableRoot.
	"first page is strong"
	page := newHeap
					allocateSlots: newHeap classTablePageSize
					format: newHeap arrayFormat
					classIndex: newHeap arrayClassIndexPun.
	self assert: (newHeap numSlotsOf: page) = newHeap classTablePageSize.
	self assert: (newHeap formatOf: tableRoot) = newHeap arrayFormat.
	self assert: (newHeap classIndexOf: tableRoot) = newHeap arrayClassIndexPun.
	self assert: (newHeap objectAfter: tableRoot limit: newHeap freeStart) = page.
	"lastClassTablePage := page."
	newHeap nilFieldsOf: page.
	newHeap storePointer: 0 ofObject: tableRoot withValue: page.
	newHeap setHiddenRootsObj: tableRoot.
	
	"this should actually take into account special indexes and immediate classes.
	Skipped for the moment"
	maxSize := (1 << self objectMemory classTableMajorIndexShift) + (self systemDefinition allClasses size * 2).
	
	numPages := (maxSize + newHeap classTableMinorIndexMask / newHeap classTablePageSize) truncated.
	2 to: numPages do:
		[:i|
		page := newHeap
					allocateSlots: newHeap classTablePageSize
					format: newHeap arrayFormat
					classIndex: newHeap arrayClassIndexPun.
		self assert: (newHeap numSlotsOf: page) = newHeap classTablePageSize.
		self assert: (newHeap formatOf: page) = newHeap arrayFormat.
		self assert: (newHeap classIndexOf: page) = newHeap arrayClassIndexPun.
		newHeap fillObj: page numSlots: newHeap classTablePageSize with: newHeap nilObject.
		newHeap storePointer: i - 1 ofObject: tableRoot withValue: page.
		self assert: (newHeap objectAfter: (newHeap fetchPointer: i - 2 ofObject: tableRoot)  limit: newHeap freeStart) = page.
		"lastClassTablePage := page"].
	"and once again to recompute numClassTablePages post building the class table."
	newHeap instVarNamed: 'numClassTablePages' put: nil.
	newHeap setHiddenRootsObj: tableRoot
]

{ #category : #running }
PBImageBuilderSpur50 >> allocateFreeLists [
	"Allocate the freeLists array."
	| newOop |
	freeListsOop := self objectMemory
						allocateSlots: objectSpace backend simulator objectMemory numFreeLists
						format: objectSpace backend simulator objectMemory wordIndexableFormat
						classIndex: objectSpace backend simulator objectMemory wordSizeClassIndexPun.
	self assert: (self objectMemory objectAfter: objectSpace backend trueObject target address) = freeListsOop.
	newOop := freeListsOop.
	self haltIf: [ newOop = 4239888 ].
	0 to: objectSpace backend simulator objectMemory numFreeLists - 1 do:
		[:i|
		self objectMemory
			storePointerUnchecked: i
			ofObject: freeListsOop
			withValue: 0].
	self objectMemory initializeFreeSpacePostLoad: freeListsOop.
]

{ #category : #'class-indexes' }
PBImageBuilderSpur50 >> arrayClassIndex [
	
	^ self objectMemory arrayClassIndexPun
]

{ #category : #'final steps' }
PBImageBuilderSpur50 >> compactImage [
	| firstFree lastUsed newHeap |
	newHeap := espellBackend objectMemory.
	newHeap allHeapEntitiesDo:
		[:o|
		(newHeap isFreeObject: o)
			ifTrue: [firstFree ifNil: [firstFree := o]]
			ifFalse: [lastUsed := o]].
	lastUsed < firstFree ifTrue: "nothing to do"
		[^self].
]

{ #category : #running }
PBImageBuilderSpur50 >> createBehaviorFromDefinition: aRFiDefinition [

	[ ^ super createBehaviorFromDefinition: aRFiDefinition ]
			on: AssertionFailure do: [ :e |
				((e signalerContext method selector == #assert:)
					and: [ e signalerContext sender method selector = #mapStackPages ]) ifFalse: [ e pass ]] 
]

{ #category : #running }
PBImageBuilderSpur50 >> createFalse [

	"False format should be changed and calculated depending on the image"
	| falseClassIndex falseObject newOop |
	falseClassIndex := self nextOrdinaryClassIndex.
	falseObject := self objectSpace backend 
		createObjectWithFormat: False format withSize: 0 classIndex: falseClassIndex.
	objectSpace falseObject: falseObject.
	^ objectSpace falseObject

]

{ #category : #running }
PBImageBuilderSpur50 >> createNil [

	"UndefinedObject format should be changed and calculated depending on the image"
	| undefinedObjectClassIndex nilObject newOop |
	undefinedObjectClassIndex := self nextOrdinaryClassIndex.
	nilObject := self objectSpace backend 
		createObjectWithFormat: UndefinedObject format withSize: 0 classIndex: undefinedObjectClassIndex.
	objectSpace nilObject: nilObject.
	^ objectSpace nilObject
]

{ #category : #running }
PBImageBuilderSpur50 >> createTrue [

	"True format should be changed and calculated depending on the image"
	| trueClassIndex trueObject newOop |
	trueClassIndex := self nextOrdinaryClassIndex.
	trueObject := self objectSpace backend 
		createObjectWithFormat: True format withSize: 0 classIndex: trueClassIndex.
	objectSpace trueObject: trueObject.
	^ objectSpace trueObject

]

{ #category : #running }
PBImageBuilderSpur50 >> createVMStubs [

	| specialObjectsArray |
	super log: 'creating initial Objects needed by the VM'.

	self createNil.	
	self createFalse.
	self createTrue.

	self allocateFreeLists.	
	self allocateClassTable.
	
	specialObjectsArray := self objectSpace backend 
		createObjectWithFormat: Array format withSize: 60 classIndex: self arrayClassIndex.
	objectSpace specialObjectsArray: specialObjectsArray.
	objectSpace nilObject: objectSpace nilObject.
	objectSpace falseObject: objectSpace falseObject.
	objectSpace trueObject: objectSpace trueObject.
	
	objectSpace nilObject setClass: (self createStubForClassNamed: #UndefinedObject).
	objectSpace falseObject setClass: (self createStubForClassNamed: #False).
	objectSpace trueObject setClass: (self createStubForClassNamed: #True).
	
	
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
	objectSpace backend floatClass: (self createStubForClassNamed: #BoxedFloat64).
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
	
	objectSpace backend processorAssociation: (self classNamed: #Association) mirror basicNew.
	objectSpace backend processorAssociation
		instanceVariableAtIndex: 2 put: (self createStubForClassNamed: #ProcessorScheduler) basicNew.
	
	objectSpace backend specialSelectors: ((self classNamed: #Array) mirror basicNew: 64)
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> errorMessagesArray [

	"return objects for error messages - special objects array at index 52"
	
 	^ {nil "nil => generic error". 
		#'bad receiver'. #'bad argument'. #'bad index'.
		#'bad number of arguments'.
		#'inappropriate operation'.  #'unsupported operation'.
		#'no modification'. #'insufficient object memory'.
		#'insufficient C memory'. #'not found'. #'bad method'.
		#'internal error in named primitive machinery'.
		#'object may move'. #'resource limit exceeded'.
		#'object is pinned'. #'primitive write beyond end of object'.
		#'object moved'. #'object not pinned'. #'callback error'} asLiteralInObjectSpace: objectSpace
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> firstOrdinaryClassIndex [
	
	^ self objectMemory classTablePageSize
]

{ #category : #running }
PBImageBuilderSpur50 >> flushNewSpace [

	| newHeap |
	self checkpoint: 'created_vm_objects'.
	
	newHeap := espellBackend objectMemory.
	newHeap initializePostBootstrap.
	
	espellBackend simulator initializeInterpreter: 0.
	espellBackend simulator instVarNamed: 'methodDictLinearSearchLimit' put: SmallInteger maxVal.
	
	"try to move nil, true and false to the new space"
	newHeap flushNewSpace.

	self
		assert: (espellBackend simulator objectMemory objectStartingAt: espellBackend simulator objectMemory oldSpaceStart)
		= objectSpace nilObject target address.

	self
		assert: (espellBackend simulator objectMemory objectAfter: objectSpace nilObject target address)
		= objectSpace falseObject target address.
		
	self
		assert: (espellBackend simulator objectMemory objectAfter: objectSpace falseObject target address)
		= objectSpace trueObject target address.
	
	objectSpace backend initializeExecutionContext.
	self prepareSimulatorForExecution.
]

{ #category : #'final steps' }
PBImageBuilderSpur50 >> followForwardingPointers [
	| newHeap |
	newHeap := espellBackend objectMemory.
	newHeap allObjectsDo: [:o|
		(newHeap isForwarded: o) ifFalse:
			[0 to: (newHeap numPointerSlotsOf: o) - 1 do:
				[:i| | field |
				field := newHeap fetchPointer: i ofObject: o.
				(newHeap isOopForwarded: field) ifTrue:
					[newHeap
						storePointer: i
						ofObject: o
						withValue: (newHeap followForwarded: field)]]]].
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> formatOfClass: aRFiClass [ 
	
	"A class format is composed by"
	"<5 bits inst spec><16 bits inst size>"
	| newHeap instSize newInstSpec |

	newHeap := objectSpace backend simulator objectMemory.

	((aRFiClass name = 'SmallInteger')
	 or: [(aRFiClass name = 'Character')
	 or: [aRFiClass name = 'SmallFloat64']]) ifTrue:
		[^ newHeap integerObjectOf: newHeap instSpecForImmediateClasses << 16].
	instSize := aRFiClass instSize.
	newInstSpec := self instSpecOfClass: aRFiClass.
	^newInstSpec << 16 + instSize
]

{ #category : #'final step' }
PBImageBuilderSpur50 >> freeForwarders [
	"Check that all forwarders have been followed.  Then free them."
	| numForwarders numFreed newHeap |
	numForwarders := numFreed := 0.
	newHeap := espellBackend objectMemory.
	newHeap allObjectsDo:
		[:o|
		(newHeap isForwarded: o)
			ifTrue: [numForwarders := numForwarders + 1]
			ifFalse:
				[0 to: (newHeap numPointerSlotsOf: o) - 1 do:
					[:i|
					self assert: (newHeap isOopForwarded: (newHeap fetchPointer: i ofObject: o)) not]]].
	Transcript cr;  nextPutAll: 'freeing '; print: numForwarders; nextPutAll: ' forwarders'; cr; flush.
	newHeap allObjectsDo:
		[:o|
		(newHeap isForwarded: o) ifTrue:
			[numFreed := numFreed + 1.
			 newHeap freeObject: o]].
	self assert: numFreed = numForwarders
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> instSpecOfClass: aRFiClass [

	"NEW:
	 0 = 0 sized objects (UndefinedObject True False et al)
	 1 = non-indexable objects with inst vars (Point et al)
	 2 = indexable objects with no inst vars (Array et al)
	 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al)
	 4 = weak indexable objects with inst vars (WeakArray et al)
	 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
	 6 = reserved
	 7 = forwarder format (also immediate class format)
	 9          64-bit indexable
	 10 - 11 32-bit indexable
	 12 - 15 16-bit indexable
	 16 - 23 byte indexable
	 24 - 31 compiled method"
	(aRFiClass isBytes) ifTrue: [ ^ 16 ].
	(aRFiClass isWords) ifTrue: [ ^ 10 ].
	(aRFiClass isCompiledMethod) ifTrue: [ ^ 24 ].
	(aRFiClass isEphemeron) ifTrue: [ ^ 5 ].
	(aRFiClass instSize = 0 and: [ aRFiClass isVariable not ]) ifTrue: [ ^ 0 ].
	(aRFiClass instSize > 0 and: [ aRFiClass isVariable not ]) ifTrue: [ ^ 1 ].
	(aRFiClass instSize = 0 and: [ aRFiClass isVariable     ]) ifTrue: [ ^ 2 ].
	(aRFiClass instSize > 0 and: [ aRFiClass isVariable     ]) ifTrue: [ ^ 3 ].
	(aRFiClass instSize > 0 and: [ aRFiClass isVariable and: [ aRFiClass isWeak ] ]) ifTrue: [ ^ 4 ].
	(aRFiClass instSize > 0 and: [ aRFiClass isVariable not and: [ aRFiClass isWeak ] ]) ifTrue: [ ^ 5 ].
	self error: 'illegal old format'
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> instantiateClassLoader [

	classLoader := PBSpurClassLoader new.
]

{ #category : #'instance creation' }
PBImageBuilderSpur50 >> newSpecialObjectsArray [
	| newSpecialObjectsArray |
	newSpecialObjectsArray := espellBackend arrayClass asClassMirror basicNew: 60.
	newSpecialObjectsArray at: 1 put: espellBackend nilObject.
	newSpecialObjectsArray at: 2 put: espellBackend falseObject.
	newSpecialObjectsArray at: 3 put: espellBackend trueObject.
	newSpecialObjectsArray
		at: 4
		put: espellBackend processorAssociation.
	newSpecialObjectsArray at: 5 put: espellBackend nilObject.
	newSpecialObjectsArray at: 6 put: espellBackend smallIntegerClass.
	newSpecialObjectsArray at: 7 put: espellBackend byteStringClass.
	newSpecialObjectsArray at: 8 put: espellBackend arrayClass.
	newSpecialObjectsArray at: 9 put: espellBackend smalltalkInstance.
	newSpecialObjectsArray at: 10 put: espellBackend floatClass.
	newSpecialObjectsArray at: 11 put: espellBackend contextClass.
	newSpecialObjectsArray at: 12 put: espellBackend nilObject.
	newSpecialObjectsArray at: 13 put: espellBackend pointClass.
	newSpecialObjectsArray at: 14 put: espellBackend largePositiveIntegerClass.
	newSpecialObjectsArray at: 15 put: espellBackend nilObject.
	newSpecialObjectsArray at: 16 put: espellBackend messageClass.
	newSpecialObjectsArray at: 17 put: espellBackend methodClass.
	newSpecialObjectsArray at: 18 put: espellBackend nilObject.
	newSpecialObjectsArray at: 19 put: espellBackend semaphoreClass.
	newSpecialObjectsArray at: 20 put: espellBackend characterClass.
	newSpecialObjectsArray at: 21 put: (#doesNotUnderstand: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 22 put: (#cannotReturn: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 23 put: espellBackend nilObject.
	newSpecialObjectsArray at: 24 put: (#(#+ 1 #- 1 #< 1 #> 1 #'<=' 1 #'>=' 1 #= 1 #'~=' 1 #* 1 #/ 1 #'\\' 1 #@ 1 #bitShift: 1 #'//' 1 #bitAnd: 1 #bitOr: 1 #at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #'==' 1 nil 0 #'~~' 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0) asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray
		at: 25
		put: espellBackend characterTable. "character table"
	newSpecialObjectsArray at: 26 put: (#mustBeBoolean asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 27 put: espellBackend byteArrayClass.
	newSpecialObjectsArray at: 28 put: espellBackend processClass.
	newSpecialObjectsArray at: 29 put: (espellBackend arrayClass basicNew: 0).
	30 to: 56 do: [ :idx | newSpecialObjectsArray at: idx put: espellBackend nilObject ].
	newSpecialObjectsArray at: 37 put: espellBackend blockClass.
	"External semaphore table"
	newSpecialObjectsArray at: 39 put: (espellBackend arrayClass asClassMirror basicNew: 20 ).
	newSpecialObjectsArray at: 43 put: espellBackend largeNegativeIntegerClass.
	newSpecialObjectsArray at: 49 put: (#aboutToReturn:through: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 50 put: (#run:with:in: asLiteralInObjectSpace: objectSpace).
	self flag: #'add WeakFinalization list'.
	newSpecialObjectsArray at: 51 put: (#attemptToAssign:withIndex: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 52 put: self errorMessagesArray.
	newSpecialObjectsArray at: 54 put: (#invokeCallbackContext: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 58 put: (#unusedBytecode asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 59 put: (#conditionalBranchCounterTrippedOn: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 60 put: (#classTrapFor: asLiteralInObjectSpace: objectSpace).

	^newSpecialObjectsArray.
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> nextOrdinaryClassIndex [
	
	| result |
	nextOrdinaryClassIndex ifNil: [ nextOrdinaryClassIndex := self firstOrdinaryClassIndex ].
	result := nextOrdinaryClassIndex.
	nextOrdinaryClassIndex := nextOrdinaryClassIndex + 1.
	^ result
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> objectMemory [
	
	^ espellBackend simulator objectMemory
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> prepareSimulatorForExecution [

	| savedEndOfMemory |
	self objectMemory initializePostBootstrap.
	
	"savedEndOfMemory := self objectMemory endOfMemory.
	self objectMemory setEndOfMemory: self objectMemory freeOldSpaceStart.
	self objectMemory setEndOfMemory: savedEndOfMemory."
"	self objectMemory interpreter initializeInterpreter: 0.
"	self objectMemory
		initializeNewSpaceVariables;
		bootstrapping: false;
		assimilateNewSegment: (self objectMemory segmentManager segments at: 0);
		setCheckForLeaks: 0;
		runLeakCheckerForFullGC.
]

{ #category : #'final steps' }
PBImageBuilderSpur50 >> reportSizes [
	| oldAvgBytes "newAvgBytes" newHeapSize newHeap |
	newHeapSize := 20 * 1024 * 1024.
	newHeap := espellBackend objectMemory.
	Transcript
		nextPutAll: 'done.'; cr;
		nextPutAll: 'initial heap size: '; nextPutAll: newHeapSize asStringWithCommas; cr;
"		nextPutAll: 'change: '; print: change * 100.0 maxDecimalPlaces: 2; nextPut: $%; cr;"
		flush.
	newHeapSize := newHeap endOfMemory
					- newHeap scavenger eden limit
					- newHeap totalFreeListBytes.
"	newAvgBytes := newHeapSize asFloat / newHeapNumObjs."
	Transcript
		nextPutAll: 'final new heap size: '; nextPutAll: newHeapSize asStringWithCommas; tab;
	"	nextPutAll: ' (avg obj bytes '; print: newAvgBytes maxDecimalPlaces: 2; nextPutAll: ' words '; print: newAvgBytes / self wordSize maxDecimalPlaces: 2; nextPut: $); cr;
""		nextPutAll: 'change: '; print: change * 100.0 maxDecimalPlaces: 2; nextPut: $%; cr;"
		flush
]

{ #category : #'as yet unclassified' }
PBImageBuilderSpur50 >> writeSnapshot: imageFileName ofTransformedImage: spurHeap headerFlags: headerFlags screenSize: screenSizeInteger [
	"The bootstrapped image typically contains a few big free chunks and one huge free chunk.
	 Test snapshot writing and loading by turning the largest non-huge chunks into segment bridges
	 and saving."
	| penultimate ultimate sim |
"	[ spurHeap  flushNewSpace. ]
		on: Halt
		do: [:ex|"
			"suppress halts from the usual suspects (development time halts)"	
"			(#(#fullGC #compactImage  #doScavenge:) includes: ex signalerContext sender selector)
				ifTrue: [ex resume]
				ifFalse: [ex pass]]."
	sim := spurHeap coInterpreter.
	sim bootstrapping: true.
	spurHeap segmentManager prepareForSnapshot.
	spurHeap
		setEndOfMemory: spurHeap endOfMemory + spurHeap bridgeSize. "hack; initializeInterpreter: cuts it back by bridgeSize"
	sim initializeInterpreter: 0;
		setImageHeaderFlagsFrom: headerFlags;
		setSavedWindowSize: (800<<16)+600;
		setDisplayForm: nil.
	spurHeap allOldSpaceEntitiesDo: [:e| penultimate := ultimate. ultimate := e].
	"Check that we've left behind the old, pre-pigCompact segmented save"
	self assert: (spurHeap isFreeObject: penultimate) not.
	spurHeap checkFreeSpace.
	spurHeap runLeakCheckerForFullGC.
	sim bereaveAllMarriedContextsForSnapshotFlushingExternalPrimitivesIf: true.
	sim imageName: imageFileName.
	sim writeImageFileIO.
	Transcript cr; show: 'Done!'
]
