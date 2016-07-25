AndreasSystemProfiler uses sub-msec VM supported PC sampling.

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Released by Ron, Julie and David

Example:
AndreasSystemProfiler spyOn: [ 10000 timesRepeat: [ 3.14159 printString ] ]

-=-=-=-=-=-=-=
Apparently, the time taken to run the provided block is as twice as long as run without the profiler.

-=-=-=-=-=-=-=
Both AndreasSystemProfiler and MessageTally are periodic sampling profilers.  The essential difference between AndreasSystemProfiler and MessageTally is in how the current method is sampled.

MessageTally is driven from a high-priority process in a loop waiting on a delay.  When the delay fires the lower-priority process being profiled is interrupted, its stack is walked to determine the methods along the call chain, and that data is recorded.  But since the sampling occurs when the high-priority process preempts the lower-priority process, a sample is only taken at a preemption point.  In particular, primitives are *not* profiled because they are not suspension points.  A process can only be suspended on method activation (a non-primitive method activation, or primitive failure) or on backward branch.  The cost of primitives is charged to a caller and is inferred by subtracting the cost of children of the caller from the caller itself (subtracting the number of samples in children of the caller form the number of samples in the caller itself).  

Another problem is that using the clock that underlies Delay, which is typically the clock used by processes being profiled, causes sampling errors due to the sampling and sampled processes cohering.  Delays are limited in resolution (at best 1 millisecond) so if the profiled process waits on a delay it'll fire immediately after the profiling process (because the profiling process is at higher priority) and so the sampling process may only ever see the sampled process in a wait state.

If MessageTally is used to profile multiple processes then a third problem is that if a primitive causes a process switch then its cost will end up being charged to the process switched-to, not switched from.  This is again because sampling can only occur after a primitive has completed (successfully or not).

AndreasSystemProfiler is driven from a high-priority process in a loop waiting on a Semaphore known to the VM.  The profiling process uses a primitive to schedule a sample some number of ticks of the VM's high-performance clock in the future.  When the time is reached the VM samples the current method and the current process, *before any process preemption takes place*, and independently of the standard clock, and signals the semaphore.  The profiling process then collects the method,process pair via primitives.  So AndreasSystemProfiler provides much more accurate results.

That said there are still limitations with primitives and Cog.  Currently Cog only samples "interpreter" primitives.  Those primitives it implements in machine code (integer and float arithmetic, closure evaluation, at:, identityHash) are not sampled and won't show up; they will be charged to the calling method.  This is fixable, since Cog actually compiles the sampling direct into interpreter primitive invocation when profiling is in effect and not at other times, but sampling could be a significant cost in these simple and performance-critical primitives.