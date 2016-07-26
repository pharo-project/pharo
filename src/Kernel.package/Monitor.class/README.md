A monitor provides process synchronization that is more high level than the one provided by a Semaphore. Similar to the classical definition of a Monitor it has the following properties:

1) At any time, only one process can execute code inside a critical section of a monitor.
2) A monitor is reentrant, which means that the active process in a monitor never gets blocked when it enters a (nested) critical section of the same monitor.
3) Inside a critical section, a process can wait for an event that may be coupled to a certain condition. If the condition is not fulfilled, the process leaves the monitor temporarily (in order to let other processes enter) and waits until another process signals the event. Then, the original process checks the condition again (this is often necessary because the state of the monitor could have changed in the meantime) and continues if it is fulfilled.
4) The monitor is fair, which means that the process that is waiting on a signaled condition the longest gets activated first.
5) The monitor allows you to define timeouts after which a process gets activated automatically.


Basic usage:

Monitor>>critical: aBlock
Critical section.
Executes aBlock as a critical section. At any time, only one process can execute code in a critical section.
NOTE: All the following synchronization operations are only valid inside the critical section of the monitor!

Monitor>>wait
Unconditional waiting for the default event.
The current process gets blocked and leaves the monitor, which means that the monitor allows another process to execute critical code. When the default event is signaled, the original process is resumed.

Monitor>>waitWhile: aBlock
Conditional waiting for the default event.
The current process gets blocked and leaves the monitor only if the argument block evaluates to true. This means that another process can enter the monitor. When the default event is signaled, the original process is resumed, which means that the condition (argument block) is checked again. Only if it evaluates to false, does execution proceed. Otherwise, the process gets blocked and leaves the monitor again...

Monitor>>waitUntil: aBlock
Conditional waiting for the default event.
See Monitor>>waitWhile: aBlock.

Monitor>>signal
One process waiting for the default event is woken up.

Monitor>>signalAll
All processes waiting for the default event are woken up.


Using non-default (specific) events:

Monitor>>waitFor: aSymbol
Unconditional waiting for the non-default event represented by the argument symbol.
Same as Monitor>>wait, but the process gets only reactivated by the specific event and not the default event.

Monitor>>waitWhile: aBlock for: aSymbol
Confitional waiting for the non-default event represented by the argument symbol.
Same as Monitor>>waitWhile:for:, but the process gets only reactivated by the specific event and not the default event.

Monitor>>waitUntil: aBlock for: aSymbol
Confitional waiting for the non-default event represented by the argument symbol.
See Monitor>>waitWhile:for: aBlock.

Monitor>>signal: aSymbol
One process waiting for the given event is woken up. If there is no process waiting for this specific event, a process waiting for the default event gets resumed.

Monitor>>signalAll: aSymbol
All process waiting for the given event or the default event are woken up.

Monitor>>signalReallyAll
All processes waiting for any events (default or specific) are woken up.


Using timeouts

Monitor>>waitMaxMilliseconds: anInteger
Monitor>>waitFor: aSymbol maxMilliseconds: anInteger
Same as Monitor>>wait (resp. Monitor>>waitFor:), but the process gets automatically woken up when the specified time has passed.

Monitor>>waitWhile: aBlock maxMilliseconds: anInteger
Monitor>>waitWhile: aBlock for: aSymbol maxMilliseconds: anInteger
Same as Monitor>>waitWhile: (resp. Monitor>>waitWhile:for:), but the process gets automatically woken up when the specified time has passed.

Monitor>>waitUntil: aBlock maxMilliseconds: anInteger
Monitor>>waitUntil: aBlock for: aSymbol maxMilliseconds: anInteger
Same as Monitor>>waitUntil: (resp. Monitor>>waitUntil:for:), but the process gets automatically woken up when the specified time has passed.