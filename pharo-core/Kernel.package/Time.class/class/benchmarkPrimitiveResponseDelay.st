benchmarks
benchmarkPrimitiveResponseDelay	"Time benchmarkPrimitiveResponseDelay"
	"Benchmark the overhead for primitive dispatches with an active Delay.
	On the VM level, this tests the efficiency of ioLowResMSecs."

	"PII/400 Windows98: 0.128 microseconds per prim"

	"ar 9/6/1999: This value is *extremely* important for stuff like sockets etc.
	I had a bad surprise when Michael pointed this particular problem out:
	Using the hardcoded clock() call for ioLowResMSecs on Win32 resulted in an overhead
	of 157.4 microseconds per primitive call - meaning you can't get no more than
	approx. 6000 primitives per second on my 400Mhz PII system with an active delay!
	BTW, it finally explains why Squeak seemed soooo slow when running PWS or 
	other socket stuff. The new version (not using clock() but some Windows function) 
	looks a lot better (see above; approx. 8,000,000 prims per sec with an active delay)."

	| nLoops bb index baseTime actualTime delayTime |
	delayTime := 5000. "Time to run this test is approx. 3*delayTime"

	Delay anyActive ifTrue:[
		^self notify:'Some delay is currently active.
Running this benchmark will not give any useful result.'].

	bb := Array new: 1. "The object we send the prim message to"

	"Compute the # of loops we'll run in a decent amount of time"
	[(Delay forMilliseconds: delayTime) wait] 
		forkAt: Processor userInterruptPriority.

	nLoops := 0.
	[Delay anyActive] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		nLoops := nLoops + 1.
	].

	"Flush the cache and make sure #basicSize is in there"
	Object flushCache.
	bb basicSize.

	"Now run the loop without any active delay
	for getting an idea about its actual speed."
	baseTime := self millisecondClockValue.
	index := nLoops.
	[index > 0] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		index := index - 1.
	].
	baseTime := self millisecondsSince: baseTime.

	"Setup the active delay but try to never make it active"
	[(Delay forMilliseconds: delayTime + delayTime) wait] 
		forkAt: Processor userInterruptPriority.

	"And run the loop"
	actualTime := self millisecondClockValue.
	index := nLoops.
	[index > 0] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		index := index - 1.
	].
	actualTime := self millisecondsSince: actualTime.

	"And get us some result"
	^((actualTime - baseTime) * 1000 asFloat / (nLoops * 10) truncateTo: 0.001) printString,
		' microseconds overhead per primitive call'