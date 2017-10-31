running
createVMStubs

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