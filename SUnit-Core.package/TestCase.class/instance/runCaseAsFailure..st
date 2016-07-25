running
runCaseAsFailure: aSemaphore
	[self resources do: [:each | each availableFor: self].
	[self setUp.
	self openDebuggerOnFailingTestMethod] ensure: [self tearDown]]
		ensure: [aSemaphore signal]