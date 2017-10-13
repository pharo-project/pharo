running
runValidationTest2
	| q sema prio pusher r cnt |

	r := Random new.
	q := AtomicSharedQueue new.
	cnt := 0.
	sema := Semaphore new.
	
	prio := Processor activePriority.
	pusher := [ sema wait. 1 to: 100 do: [:i | q nextPut: i ]. Processor yield ].
	
	10 timesRepeat: [
		| proc |
		proc := pusher newProcess priority: prio + (r next * 10) asInteger  - 5.
		proc resume.
	].

	" let them run "
	10 timesRepeat: [ sema signal ].
	Processor yield.
	
	[ q next. cnt := cnt + 1. cnt < 1000 ] whileTrue.
"
	started := Time now asSeconds. 
	[
	[ count < (1000) ] whileTrue: [
		Time now asSeconds - started > 20 ifTrue: [ self error: 'deadlock' ].
	 Processor yield ].
	] ensure: [ 
		feeders do: [:ea | ea terminate ]
	]
"