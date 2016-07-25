signaledexception tests
simpleIsNestedTest
	"uses resignalAs:"

	[ self doSomething.
	MyTestError signal.
	self doSomethingElse 
	] on: MyTestError do: [ :exception |
		"expecting to detect handler in #runTest:"
		exception isNested 
			ifTrue: [
				self doYetAnotherThing.
				exception resignalAs: MyTestNotification new ]]