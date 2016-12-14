running
createVMStubs

	self log: 'creating initial Objects needed by the VM'.
	
	objectSpace backend specialObjectsArray: (objectSpace objectMirrorOn: (Array new: 59)).
	self createNil.
	1 to: 59 do: [ :i |
		objectSpace backend specialObjectsArray at: i put: objectSpace nilObject ].
	
	self createFalse.
	self createTrue.
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
	self checkpoint: 'created_vm_objects'.