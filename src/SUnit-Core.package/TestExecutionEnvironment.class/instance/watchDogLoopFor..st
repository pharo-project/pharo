controlling
watchDogLoopFor: testProcess

	| timeIsGone |
	[	"waiting new test case" 
		watchDogSemaphore wait. 
		"waiting while test completes"
		[timeIsGone := watchDogSemaphore waitTimeoutMSecs: maxTimeForTest asMilliSeconds.
		testCompleted] whileFalse: [ 
			"this subloop allows to dynamically change time limit and restart watch dog"
			timeIsGone ifTrue: [testProcess signalException: TestTookTooMuchTime new]	].
	] repeat.

