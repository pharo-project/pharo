running
createVMStubs

	super createVMStubs.
	objectSpace backend smallFloatClass: (self createStubForClassNamed: #SmallFloat64).