DelayExperimentalSemaphoreScheduler addresses a "Delay scheduling deadlock" problem of Case 14344 [1] seemingly due to multi-thread suspend/resume operations interacting badly with background morphs, causing the Image to lock up.  See the class comment of DelaySchedulerBackgroundWorkerMorph.

My #schedule: and #unschedule: methods pass Delays from the user priority threads to the high priority #handleTimerEvent thread via  instance variables /scheduledDelay/ and /finishedDelay/.   The original implementation used mutexes to protect these in a shared-memory paradigm.   However it can also be considered a producer-consumer paradigm, so I throw away the mutexes and use semaphores. 

I have been demostrated to work without error,  however but am "experimental" due to limited time to test prior to Pharo 4 Release, and also no tests are included.

I can be selected via "World > System > Settings > Setting > Delay scheduler".

To compare the different delay schedulers...
   1.   "World > Tools > Process Browser", and set to auto-update.
   2.   Transcript open.
   3.   DelayBenchmark runAll.