controlling
watchDogLoopFor: testProcess

	| timeIsGone |
	[	"waiting new test case" 
		watchDogSemaphore wait. 
		"waiting while test completes"
		[timeIsGone := watchDogSemaphore waitTimeoutMSecs: maxTimeForTest asMilliSeconds.
		testCompleted] whileFalse: [ 
			"this subloop allows to dynamically change time limit and restart watch dog"
			timeIsGone ifTrue: [
				"The main purpose of following condition is to ignore timeout when test is under debug.
				Test process is suspended only when it is debugged"
				testProcess isSuspended ifFalse: [
					testProcess signalException: TestTookTooMuchTime new]]	].
	] repeat

