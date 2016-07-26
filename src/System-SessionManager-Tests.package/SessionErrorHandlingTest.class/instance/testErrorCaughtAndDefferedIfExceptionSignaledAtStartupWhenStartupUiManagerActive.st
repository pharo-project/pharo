tests
testErrorCaughtAndDefferedIfExceptionSignaledAtStartupWhenStartupUiManagerActive
	manager register: (TestSessionHandler onStartup: [ Halt now ]).	
	session errorHandler: 
		(TestStartupUIManager new
			sessionManager: manager;
			yourself).
	
	self 
		shouldnt: [ session start: false ]
		raise: Halt.
	self
		assert: (session instVarNamed: 'deferredStartupActions') size 
		equals: 1