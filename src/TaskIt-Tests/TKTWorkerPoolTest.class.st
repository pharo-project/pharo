Class {
	#name : #TKTWorkerPoolTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Worker'
}

{ #category : #tests }
TKTWorkerPoolTest >> testPriorityAfterStart [
	| pool workerProcess newPriority doneSemaphore|
	newPriority := 32.
	pool := TKTWorkerPool new.
	pool name: 'Test pool'.
	pool poolMaxSize: 4.
	pool start.
	pool schedule: (TKTTask valuable: [Semaphore new wait]).
	doneSemaphore := Semaphore new.
	pool schedule: (TKTTask valuable: [doneSemaphore signal]).
	doneSemaphore wait: 500 milliSeconds.
	pool priority: newPriority.
	self assert: pool priority equals: newPriority.
	pool workers do: [: each | 
		workerProcess :=  each process process.
		self assert: workerProcess priority equals: newPriority].
	
]

{ #category : #tests }
TKTWorkerPoolTest >> testPrioritySetBeforeStart [
	| pool workerProcess initialPriority doneSemaphore |
	initialPriority := 32.
	pool := TKTWorkerPool new.
	pool name: 'Test pool'.
	pool poolMaxSize: 4.
	pool priority: initialPriority.
	self assert: pool priority equals: initialPriority.
	pool start.
	pool schedule: (TKTTask valuable: [Semaphore new wait]).
	doneSemaphore := Semaphore new.
	pool schedule: (TKTTask valuable: [doneSemaphore signal]).
	doneSemaphore wait: 500 milliSeconds.
	pool workers do: [: each | 
		workerProcess :=  each process process.
		self assert: workerProcess priority equals: initialPriority].
	
]

{ #category : #tests }
TKTWorkerPoolTest >> testWorkerPoolCretesWorkersWhenExecutingATask [
	| pool |
	pool := TKTWorkerPool new.
	pool name: 'test pool'.
	pool poolMaxSize: 4.
	pool start.
	(pool future: [ 1 ]) waitForCompletion: 1 second.
	self assert: pool workers notEmpty
]

{ #category : #tests }
TKTWorkerPoolTest >> testWorkerPoolDoesNotExceedPoolSize [
	| pool |
	pool := TKTWorkerPool new.
	pool poolMaxSize: 4.
	pool start.
	((1 to: 10) collect: [ :i | pool future: [ 1 + 1 ] ])
		do: [ :future | future waitForCompletion: 1 second ].
	self assert: pool size <= 4
]

{ #category : #tests }
TKTWorkerPoolTest >> testWorkerPoolDoesNotExceedPoolSizeWhenSchedulingTasksInParallel [

	| pool totalTasks futures |
	pool := TKTWorkerPool new.
	pool poolMaxSize: 10.
	pool start.
	totalTasks := 0.
		
	futures := AtomicSharedQueue new.
	1 to: 10 do: [:i | 
		futures nextPut: (pool future: [ 
			10 timesRepeat: [ futures nextPut: (pool future: [
					totalTasks := totalTasks + 1 ]) ] ]) ].
	
	[ futures isEmpty ] whileFalse: [ futures next waitForCompletion: 1 second ].
	
	self assert: totalTasks equals: 100.
	self assert: pool size equals: 10
]

{ #category : #tests }
TKTWorkerPoolTest >> testWorkerPoolStopStopsWorkers [

	| pool |
	pool := TKTWorkerPool new.
	pool poolMaxSize: 4.
	pool start.
		
	((1 to: 10) collect: [:i | pool future: [ 1 + 1 ] ])
		do: [ :future | future waitForCompletion: 500 milliSeconds. ].
	
	pool stop.
	
	pool workers do: [ :worker | self deny: worker isRunning ].
]

{ #category : #tests }
TKTWorkerPoolTest >> testWorkerPoolWorkersAreStoppedAfterPoolsCollection [

	| pool workerProcess |
	pool := TKTWorkerPool new.
	pool name: 'test pool'.
	pool poolMaxSize: 4.
	pool start.

	(pool future: [ 1 ]) waitForCompletion: 1 second.

	self assert: pool workers notEmpty.

	workerProcess := WeakArray with: pool workers anyOne process.

	pool := nil.
	self assertWithGarbageCollect: [ workerProcess first isNil ].
]

{ #category : #tests }
TKTWorkerPoolTest >> testWorkerPoolWorkersProcessesAreStoppedAfterPoolsCollection [

	| pool workerProcess |
	pool := TKTWorkerPool new.
	pool name: 'test pool'.
	pool poolMaxSize: 4.
	pool start.

	(pool future: [ 1 ]) waitForCompletion: 1 second.

	workerProcess := WeakArray with: pool workers anyOne process process.

	pool := nil.
	self assertWithGarbageCollect: [ workerProcess first isNil ].
]

{ #category : #tests }
TKTWorkerPoolTest >> testWorkerPoolWorkersProcessesAreTerminatedAfterPoolsCollection [

	| pool workerProcess |
	pool := TKTWorkerPool new.
	pool name: 'Test pool'.
	pool poolMaxSize: 4.
	pool start.
	(pool future: [ 1 ]) waitForCompletion: 1 second.
	workerProcess := pool workers anyOne process process.
	pool := nil.
	self assertWithGarbageCollect: [ workerProcess isTerminated ].

]
