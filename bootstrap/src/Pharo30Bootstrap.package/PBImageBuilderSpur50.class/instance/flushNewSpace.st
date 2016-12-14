running
flushNewSpace

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