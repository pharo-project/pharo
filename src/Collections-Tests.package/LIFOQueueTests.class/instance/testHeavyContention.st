tests
testHeavyContention
	"run 10 threads, pushing new values to queue,
	and 10 threads pullung values from queue,
	at random priorities"
	
	| q sema prio pusher feeder
		feeders r crit done count |

	r := Random new.
	q := self newQueue.
	feeders := OrderedCollection new.
	count := 0.
	sema := Semaphore new.
	crit := Semaphore forMutualExclusion.
	done := Semaphore new.
	
	prio := Processor activePriority.
	pusher := [ sema wait. 1 to: 100 do: [:i | q nextPut: i ]. ].
	feeder := [ sema wait. 
		[ q nextOrNil ifNotNil: [ crit critical: [count := count + 1 ]]. Processor yield. count < 1000 ] whileTrue. done signal ].
	
	10 timesRepeat: [
		| proc |
		proc := pusher newProcess priority: prio + (r next * 10) asInteger.
		proc resume.
		"run feeders at lower priority, otherwise they won't give a chance pushers to complete,
		because queue doesn't blocks the process"
		proc := feeder newProcess priority: prio + (r next * 10) asInteger  - 10.
		feeders add: proc. 
		proc resume.		
	].

	" let them run "
	20 timesRepeat: [ sema signal ].
	Processor yield.
	
	done waitTimeoutSeconds: 10.
	 
	feeders do: [:ea | ea terminate ].
	self assert: (count = 1000 ).
	self assert: q nextOrNil == nil