running
createVMStubs

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