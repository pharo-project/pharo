tests
testMutualExclusion

	| lock steps |
	steps := OrderedCollection new.
	lock := Semaphore forMutualExclusion.
	
	[
		steps add: #startProcess1.
		lock critical: [
			steps add: #startCritical1.
			"waiting next process run"
			[steps includes: #startProcess2] whileFalse: [Processor yield].
			steps add: #endCritical1 ] ] fork.
	[
		"Waiting first process run"
		[steps includes: #startProcess1] whileFalse: [Processor yield].
		steps add: #startProcess2.
		lock critical: [ 
			steps add: #startCritical2.
			Processor yield.
			steps add: #endCritical2 ] ] fork.
	
	[ steps size = 6 ] whileFalse: [ Processor yield ].

	self assert: (steps hasEqualElements: #(startProcess1 startCritical1 startProcess2 endCritical1 startCritical2 endCritical2))