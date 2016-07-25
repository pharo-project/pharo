tests
testHeavyContention
	"run 10 threads, pushing new values to queue,
	and 10 threads pullung values from queue,
	at random priorities"
	
	| q sema prio pusher feeder
		feeders r crit done |

	r := Random new.
	q := AtomicSharedQueue new.
	feeders := OrderedCollection new.
	count := 0.
	sema := Semaphore new.
	crit := Semaphore forMutualExclusion.
	done := Semaphore new.
	
	prio := Processor activePriority.
	pusher := [ sema wait. 1 to: 100 do: [:i | q nextPut: i ]. ].
	feeder := [ sema wait. 
		[ q next.  crit critical: [count := count + 1 ]. count < 1000 ] whileTrue. done signal ].
	
	10 timesRepeat: [
		| proc |
		proc := pusher newProcess priority: prio + (r next * 10) asInteger.
		proc resume.
		
		proc := feeder newProcess priority: prio + (r next * 10) asInteger.
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