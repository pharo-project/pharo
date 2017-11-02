"
This concrete subclass of DelayScheduler immediately signals the delaySemaphore, effectively eliminating the delay.  This can be useful to facilitate modifications to the DelayScheduler code hierarchy.

The delay scehduler is in continual use every 20 - 50 milliseconds due to the UI loop, so it can be difficult to modify (particularly via a continuous integration validation service).  DelayNullScheduler is provided as a substitute to allow the UI loop to continue while the delay scheduler system is taken offline for modifications. If that proves insufficient, try temporarily bypassing the call to #interCyclePause from WorldState>>doOneCycleNow.

Select either from ""System > Settings > System > Delay Scheduler"" 
or do... ""Delay delaySchedulerClass: DelayNullScheduler""



"
Class {
	#name : #DelayNullScheduler,
	#superclass : #DelayScheduler,
	#category : 'Kernel-Processes'
}

{ #category : #'low priority processes' }
DelayNullScheduler >> schedule: aDelay [
	aDelay delaySemaphore signal.
	

]

{ #category : #snapshotting }
DelayNullScheduler >> shutDown [
	"do nothing"
]

{ #category : #'timer process' }
DelayNullScheduler >> startTimerEventLoop [
	"Start a fake timer event loop that does nothing - just suspends via a wait that is never signalled"
	"Delay restartTimerEventLoop"
		
	timerEventLoop ifNotNil: [ self error: 'Timer event loop has already been started.' ].
		
	timerEventLoop := [ Processor activeProcess suspend. ] newProcess.
	timerEventLoop 
		name: 'WARNING! Delay Scheduling Process NOT RUNNING!';
		priority: Processor timingPriority.
	timerEventLoop resume.

]

{ #category : #snapshotting }
DelayNullScheduler >> startUp [
	self error: 'Delay scheduler is NOT RUNNING!'.
	"The running delay scheduler seems to be DelayNullScheduler.
	 This is probably not what you want. See Tools > Process Browser.
	 
	 If all is well, the following should result in a value 1 second...
	    [1 second asDelay wait] timeToRun

	 To get delays working again, go to System > Settings > System > Delay scheduler.	
	 "
 
]

{ #category : #'timer process' }
DelayNullScheduler >> stopTimerEventLoop [
	"Stop the timer event loop"

	timerEventLoop ifNotNil: [ timerEventLoop terminate ]. 
	timerEventLoop := nil.


]

{ #category : #'low priority processes' }
DelayNullScheduler >> unschedule: aDelay [
	"Nothing to do"
		

]
