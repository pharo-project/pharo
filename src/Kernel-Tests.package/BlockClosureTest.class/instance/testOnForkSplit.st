tests - on-fork
testOnForkSplit
	"Test that when forking, the stack are split correctly (there is no any contexts referenced by both processes)"
	
	| sema timeout forkedContexts myContexts c |
	
	self flag: 'This test is too brittle, failing often on CI'; skip.
		
	sema := Semaphore new.
	
	[ 1/0 ] on: Exception fork: [  | ctx |
		forkedContexts := IdentitySet new.
		ctx := thisContext.
		[ ctx notNil ] whileTrue: [ forkedContexts add: ctx. ctx := ctx sender ].
		sema signal ].
	
	timeout := (sema waitTimeoutSeconds: 1).
	self assert: timeout == false description: 'fork lasted more than one second'.

	myContexts := IdentitySet new.
	c := thisContext.
	[ c notNil ] whileTrue: [ myContexts add: c. c := c sender ].

	self assert: (myContexts noneSatisfy: [:b | forkedContexts includes: b ]) description: 'myContexts are not within forkedContexts'.
	self assert: (forkedContexts noneSatisfy: [:b | myContexts includes: b ]) description: 'forkedContexts are not within myContexts'.