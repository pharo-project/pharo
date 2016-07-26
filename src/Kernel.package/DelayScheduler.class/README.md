I provide the high priority process that interacts with the VM via /timingSempahore/ to manage scheduling of Delays.  

The original pre-2014 code on the class-side of Delay was refactored to a singleton pattern in DelayScheduler, then further refactored into two subclasses...
 * DelayMillisecondScheduler (pre-2014, 6 day clock rollover)
 * DelayMicrosecondScheduler (new, 50,000 year rollover)
A fair amount of code is duplicated in the subclasses for both performance and helping with the live transition to the new code (some cleanup required after the new code has been in use for a while)

You can switch between different DelayScheduler implementations using Settings Browser > System. 
You can observe which delay scheduler is running using Tools > Process Browser.

Original word of advice:
DelayScheduler is THE highest priority code which is run in Pharo.  In other words it is time-critical. The speed of this code is critical for accurate responses, it is critical for network services, it affects every last part of the system. Don't fix it if it ain't broken! This code isn't supposed to be beautiful, it's supposed to be fast! The reason for duplicating code is to make it fast. The reason for not using ifNil:[]ifNotNil:[] is that the compiler might not inline those. Since the effect of changes are VERY hard to predict it is best to leave things as they are for now unless there is an actual need to change anything.

Revised word of advice:
Now you can swap between different schedulers on the fly, have a go. Create your own subclass of  DelayScheduler and use DelayBenchmark to compare the alternatives.  

Tip: If while experimenting you have trouble with the UI locking (which prevents debugging), in WorldState>>doOneCycleFor: comment out the call to #interCyclePause: .

