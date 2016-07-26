tests
testErrorHandledIfExceptionSignaledAtShutdownWhenStartupUiManagerActive
	manager register: (TestSessionHandler onShutdown: [ Halt now ]).	
	session errorHandler: 
		(TestStartupUIManager new
			sessionManager: manager;
			yourself).
	
	self 
		shouldnt: [ session stop: false ]
		raise: Halt.
	self
		assert: (session instVarNamed: 'deferredStartupActions') size 
		equals: 1