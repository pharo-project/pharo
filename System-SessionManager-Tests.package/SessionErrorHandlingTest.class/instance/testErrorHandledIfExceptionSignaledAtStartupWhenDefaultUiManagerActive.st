tests
testErrorHandledIfExceptionSignaledAtStartupWhenDefaultUiManagerActive
	"session errorHandler will be TestWorkingSession that will register caught errors"
	manager register: (TestSessionHandler onStartup: [ Halt now ]).

	"ensure error handled"
	self 
		shouldnt: [ session start: false ]
		raise: Halt.
	self
		assert: session errorHandler errors size
		equals: 1.
	
	"ensure no defered actions"
	self assert:
		(session instVarNamed: 'deferredStartupActions') isEmpty