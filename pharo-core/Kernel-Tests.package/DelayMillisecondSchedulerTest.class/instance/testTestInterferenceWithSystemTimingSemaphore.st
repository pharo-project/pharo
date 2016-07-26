tests
testTestInterferenceWithSystemTimingSemaphore
	| scheduler delay higherPriorityProcess delayExpired |
	
	self skip.
	
	"Just clarifying how the primitive works.  If our test case methods schedule any delays, 
	the consenquent call to #primSignal:atMilliseconds: replaces the system timingSemaphore, 
	preventing it being triggered to expire normal system delays, which would lead totestScheduler 
	system lockup.  This test indicates that timingSemaphore must be triggered to reset in #tearDown."
	
	scheduler := DelayMillisecondScheduler new.
	
	"FIRST SHOW HOW IT WORKS PROPERLY"
	delay := Delay forMilliseconds: 100.
	higherPriorityProcess := 
		[	delayExpired := false. 
			delay wait.
			delayExpired := true.
			] newProcess
				priority: Processor activeProcess priority + 1 ;
				name: 'DelaySchedulerTest'.		
	higherPriorityProcess resume. "The higher priority process sets delayExpired to false, then waits."
	self deny: delayExpired.		
	self busyWaitMilliseconds: 200.  
	self assert: delayExpired.		"After the wait, delayExpired was set to true"
	
	"THEN SHOW HOW IT BREAKS"
	[	
		delay := Delay forMilliseconds: 100.
		higherPriorityProcess := 
			[	delayExpired := false. 
				delay wait.
				delayExpired := true.
				] newProcess
					priority: Processor activeProcess priority + 1 ;
					name: 'DelaySchedulerTest'.		
		higherPriorityProcess resume. "The higher priority process sets delayExpired to false, then waits."
		self deny: delayExpired.		
		scheduler primSignal: Semaphore new atMilliseconds: 1. 	"But the timingSemaphore is replaced."	
		self busyWaitMilliseconds: 200.  
		self deny: delayExpired.		"So the system timer event loop is never signalled to expire the delay"
	] 
	ensure: 
	[	"Clean up and trigger original system timingSemaphore"
		Delay testCaseSupportTimingSemaphore signal. "restart system timer event loop to expire the delay."
		self assert: delayExpired. 
		higherPriorityProcess terminate.
	].

