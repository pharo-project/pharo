signaledexception tests
doublePassOuterTest
	"uses #resume"

	[[[self doSomething.
	MyTestNotification signal.
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex pass.
			self doSomethingExceptional]]
			on: MyTestNotification
			do: [:ex | ex outer.
				self doSomethingElse]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume]