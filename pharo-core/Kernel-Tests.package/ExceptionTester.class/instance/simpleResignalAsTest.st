signaledexception tests
simpleResignalAsTest
	"ExceptionTester new simpleResignalAsTest"

	[self doSomething.
	MyTestNotification signal.
	self doSomethingElse]
		on: MyTestNotification
		do:
			[:ex | ex resignalAs: MyTestError new]