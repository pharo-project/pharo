"
SUnit tests for FIFO queues
"
Class {
	#name : #FIFOQueueTests,
	#superclass : #TestCase,
	#instVars : [
		'count'
	],
	#category : 'Collections-Tests-Atomic'
}

{ #category : #'instance creation' }
FIFOQueueTests >> newQueue [

	^ WaitfreeQueue new
]

{ #category : #running }
FIFOQueueTests >> runValidationTest [
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
		[ q next. 
		crit critical: [count := count + 1 ]. count < 1000 ] whileTrue. done signal ].
	
	10 timesRepeat: [
		| proc |
		proc := pusher newProcess priority: prio + (r next * 10) asInteger  - 5.
		proc resume.
		proc := feeder newProcess priority: prio + (r next * 10) asInteger  - 10.
		feeders add: proc. 
		proc resume.		
	].

	" let them run "
	20 timesRepeat: [ sema signal ].
	Processor yield.
	
	done waitTimeoutSeconds: 10.
	 
	feeders do: [:ea | ea terminate ].
	self assert: (count = 1000 ) 

]

{ #category : #running }
FIFOQueueTests >> runValidationTest2 [
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
]

{ #category : #tests }
FIFOQueueTests >> testBasics [
	| q |
	q := self newQueue.
	
	q nextPut: 5.
	
	self assert: q peek = 5.
	self assert: q nextOrNil = 5.
	
	q nextPut: 10.
	q nextPut: 15.
	
	self assert: q nextOrNil = 10.
	self assert: q peek = 15.
	self assert: q nextOrNil = 15.
	
	self assert: q nextOrNil == nil
]

{ #category : #tests }
FIFOQueueTests >> testContention1 [
	"here is a test case that breaks the standard SharedQueue from Squeak 3.8"

	| q r1 r2 |
	q := AtomicSharedQueue new.
	q nextPut: 5.
	q nextPut: 10.

	self should: [ q nextOrNil = 5 ].

	[ r1 := q next ] fork.
	[ r2 := q next ] fork.
	Processor  yield.   "let the above two threads block"
	
	q nextPut: 10.
	Processor yield.

	self should: [ r1 = 10 ].
	self should: [ r2 = 10 ].
	self should: [ q nextOrNil = nil ].

]

{ #category : #tests }
FIFOQueueTests >> testFlush [
	| q v sema |
	q := self newQueue.
	sema := Semaphore new.
	
	[ #( 1 2 3 4 5 6 7 8 9 10 ) do: [:each |
		q nextPut: each ]. sema signal ] fork.
	
	v := 0.
	
	sema wait. 
	q flush: [:each | self assert: v < each. v := each ].

	self assert: v = 10	
]

{ #category : #tests }
FIFOQueueTests >> testFlushAllSuchThat [
	| q |
	q := self newQueue.

	
	#( 1 2 3 4 5 6 7 8 9 10 ) do: [:each | q nextPut: each ]. 
	
	q flushAllSuchThat: [:each | each odd ].
	
	q flush: [:each | self assert: each even ].
	
	self assert: q nextOrNil == nil
]

{ #category : #tests }
FIFOQueueTests >> testHeavyContention [
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
]

{ #category : #tests }
FIFOQueueTests >> testHeavyContention2 [
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
		[ q waitForNewItems. q next. crit critical: [count := count + 1 ]. count < 1000 ] whileTrue. done signal ].
	
	10 timesRepeat: [
		| proc |
		proc := pusher newProcess priority: prio + (r next * 10) asInteger - 5.
		proc resume.
		proc := feeder newProcess priority: prio + (r next * 10) asInteger - 5.
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

]

{ #category : #tests }
FIFOQueueTests >> testNextOrNilSuchThat [
	| q item |
	q := self newQueue.
	q nextPut: 5.
	q nextPut: 6.

	item := q nextOrNilSuchThat: [ :x | x even ].
	self should: [ item = 6 ].

	self should: [ q nextOrNil = 5 ].
	self should: [ q nextOrNil = nil ].

]

{ #category : #tests }
FIFOQueueTests >> testSize [
	| q |
	q := self newQueue.
	
	#( 1 2 3 4 5 6 7 8 9 10 ) do: [:each |
		q nextPut: each ].

	self assert: (q size = 10)
]

{ #category : #tests }
FIFOQueueTests >> testSuchThat [
	| q |
	q := self newQueue.
	
	#( 1 2 3 4 5 6 7 8 9 10 ) do: [:each |
		q nextPut: each ].

	self assert: (q nextOrNilSuchThat: [:e | e = 100 ]) isNil.	

	self assert: (q nextOrNilSuchThat: [:e | e = 5 ]) = 5.	

	9 timesRepeat: [ 
		self assert: (q nextOrNil notNil) ].
	
	self assert: q nextOrNil isNil
]
