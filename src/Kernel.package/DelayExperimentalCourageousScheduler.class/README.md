I determine the resume time of Delays, then schedule them to wake up at the appropriate time.  I consist of two inter-coupled parts:

  1. a "timing-priority" side (i.e. thread running at the very highest priority in the system) that maintains the suspendedDelays Heap data structure containing Delays ordered on resume times, and at the appropriate time signal each Delay's  /delaySemaphore/.  Related methods belong to the "timer process" protocol.

  2. an "application-priority" side used by application code, running at the same priority as the calling code, which pass Delays to the timing-priority side. Related methods belong to the "low priority processes" protocol.

The two sides operate in a multi-producer single-consumer paradigm synchronised via /timingSemaphore/.  Only the timing-priority thread waits on /timingSemaphore/, while this is signalled by many application-priority processes as well as the VM.

The timing-priority event loop cycles in #runTimerEventLoop, sleeping until /timingSemaphore/ is signalled, either from the VM at a time previously set by  #primSignal: atUTCMicroseconds: , or from application-priority code via #schedule: or #unschedule.  Actual processing is done in #handleTimerEvent:.

To schedule a Delay, an application thread passes it via #schedule: into the /scheduledDelay/ variable, then signals a /timingSemaphore/ event that wakes up the timing-priority thread to process the /scheduledDelay/.  The system's implicit cooperative multiasking ensures no task-switch can occur between /scheduleDelay/ being set and timingSemaphore is signalled, which ensures /scheduledDelay/ is cleared to nil by the timing-priority thread before any other thread can overwrite /scheduledDelay/.

I can be selected via "World > System > Settings > Setting > Delay scheduler".

To compare the different delay schedulers...
   1.   "World > Tools > Process Browser", and set to auto-update.
   2.   Transcript open.
   3.   DelayBenchmark runAll.