tests
testPrintOn2
	| string |
	delivery recorder: self.
	delivery preSuspendingAction: [ semaphore signal ].
	delivery activate.	
	self assertSemaphore.
	200 milliSeconds wait.
	string := String streamContents: [ :stream | delivery printOn: stream ].
	self assert: string notEmpty.
	self assert: (string includesSubstring: 'next cycle in').