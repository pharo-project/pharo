This concrete subclass of DelayScheduler immediately signals the delaySemaphore, effectively eliminating the delay.  This can be useful to facilitate modifications to the DelayScheduler code hierarchy.

The delay scehduler is in continual use every 20 - 50 milliseconds due to the UI loop, so it can be difficult to modify (particularly via a continuous integration validation service).  DelayNullScheduler is provided as a substitute to allow the UI loop to continue while the delay scheduler system is taken offline for modifications. If that proves insufficient, try temporarily bypassing the call to #interCyclePause from WorldState>>doOneCycleNow.

Select either from "System > Settings > System > Delay Scheduler" 
or do... "Delay delaySchedulerClass: DelayNullScheduler"


