"
I represent the simplest delay scheduler.  I'm so simple I actually don't schedule delays,
but signal them to continue immediately so there is no delay. 
My purpose is provide the minimum interface required to avoid DNU errors in the rest of the system.

My responsibilities are: 
1. Facilitate image shutdown/startup   
2. Facilitate the start/stop in transitioning between delay schedulers 
3. Immediately signal delays to continue to avoid user code blocking forever on unscheduled delays 

My main collaborators are:
Delay - has a class variable ""Scheduler"" storing the current delay scheduler
      - changes Scheduler by stopping old timer event loop and starting new timer event loop
      - invokes #schedule: and #unschedule: on current Scheduler
      - forwards system #startUp and #shutDown to current Scheduler


USER-API 

#schedule: aDelay
	Immediately signals the delaySemaphore to continue.
	Required by Delay>>schedule & Delay>>wait. 

#unschedule: aDelay
	Empty method body 
 	Required by Delay>>unschedule.


SYSTEM-API
	
#startTimerEventLoop
	Empty method body 
	Required by Delay-class>>#delaySchedulerClass: 
	
#stopTimerEventLoop
	Empty method body 
 	Required by Delay-class>>#delaySchedulerClass: 
		
#startUp
	Empty method body 
	Required by Delay-class>>#startup 
					
#shutDown
	Empty method body 
	Required by Delay-class>>#shutdown
"
Class {
	#name : #DelayNullScheduler,
	#superclass : #Object,
	#category : #'Kernel-Delays'
}

{ #category : #'user-api' }
DelayNullScheduler >> schedule: aDelay [
	"Invoked from user code Delay>>schedule:
	 DelayNullScheduler does not schedule delays, but need to avoid user code blocking.
	 Signal its semaphore immediately, so effectively there is no delay.
	 For example,  [1 second asDelay wait] timeToRun  ==> 0:00:00:00.000 instead of 0:00:00:01.000"
	
	aDelay delaySemaphore signal.

]

{ #category : #'system-api' }
DelayNullScheduler >> shutDown [
	"Nothing to do for DelayNullScheduler"
]

{ #category : #'system-api' }
DelayNullScheduler >> startTimerEventLoop [
	"Nothing to do for DelayNullScheduler"
]

{ #category : #'system-api' }
DelayNullScheduler >> startUp [
	Warning signal: 'Delay scheduler is NOT RUNNING!'.
	
	"To get delays working again, go to System > Settings > System > Delay scheduler. "
 
]

{ #category : #'system-api' }
DelayNullScheduler >> stopTimerEventLoop [
	"Nothing to do for DelayNullScheduler"
]

{ #category : #'user-api' }
DelayNullScheduler >> unschedule: aDelay [
	"Nothing to do for DelayNullScheduler"		

]
