testing
testShouldIgnoreTimeLimitWhenTestProcessIsSuspended
	"If you open debugger on test (by halt or error) and will not close it more then test time limit then following interaction with debugger will fail. 
	As simple fix watch dog should check that test process is not suspended. It of course will open possibility to hang test execution when tested code will suspend active process by incident. But we could live with it and probably it could be addressed too in future"
	| testProcess |
	self timeLimit: 30 milliSeconds.
	testProcess := Processor activeProcess.
	[ 50 milliSeconds wait. testProcess resume ] fork.
	testProcess suspend.
	self assert: true.