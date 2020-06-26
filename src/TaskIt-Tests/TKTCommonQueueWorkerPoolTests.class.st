Class {
	#name : #TKTCommonQueueWorkerPoolTests,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Worker'
}

{ #category : #tests }
TKTCommonQueueWorkerPoolTests >> testWorkerPoolCretesWorkersWhenExecutingATask [
	| pool |
	pool := TKTCommonQueueWorkerPool new.
	pool name: 'test pool'.
	pool poolMaxSize: 4.
	pool start.
	(pool future: [ 1 ]) waitForCompletion: 1 second.
	self assert: pool workers notEmpty
]

{ #category : #tests }
TKTCommonQueueWorkerPoolTests >> testWorkerPoolDoesNotExceedPoolSize [
	| pool |
	pool := TKTCommonQueueWorkerPool new.
	pool poolMaxSize: 4.
	pool start.
	((1 to: 100) collect: [ :i | pool future: [ 1 + 1 ] ]) do: [ :future | future waitForCompletion: 1 second ].
	1 second wait.
	self assert: pool size <=  4
]

{ #category : #tests }
TKTCommonQueueWorkerPoolTests >> testWorkerPoolDoesNotExceedPoolSizeWhenSchedulingTasksInParallel [
	| pool totalTasks futures |
	pool := TKTCommonQueueWorkerPool new.
	pool poolMaxSize: 10.
	pool start.
	totalTasks := 0.
	futures := AtomicSharedQueue new.
	1 to: 10 do: [ :i | futures nextPut: (pool future: [ 10 timesRepeat: [ futures nextPut: (pool future: [ totalTasks := totalTasks + 1 ]) ] ]) ].
	[ futures isEmpty ] whileFalse: [ futures next waitForCompletion: 100 milliSeconds ].
	self assert: totalTasks equals: 100.
	self assert: pool size equals: 10
]

{ #category : #tests }
TKTCommonQueueWorkerPoolTests >> testWorkerPoolStopStopsWorkers [

	| pool |
	pool := TKTCommonQueueWorkerPool new.
	pool poolMaxSize: 4.
	pool start.
		
	((1 to: 10) collect: [:i | pool future: [ 1 + 1 ] ])
		do: [ :future | future waitForCompletion: 500 milliSeconds. ].
	
	pool stop.
	
	pool workers do: [ :worker | self deny: worker isRunning ].
]

{ #category : #tests }
TKTCommonQueueWorkerPoolTests >> testWorkerPoolWorkersAreStoppedAfterPoolsCollection [
	<expectedFailure>
	| pool workerProcess |
	pool := TKTCommonQueueWorkerPool new.
	pool name: 'test pool' , UUID new asString.
	pool poolMaxSize: 4.
	pool start.
	(pool future: (MessageSend receiver: self selector: #yourself))
		waitForCompletion: 1 second.
	self assert: pool workers notEmpty.
	workerProcess := WeakArray with: pool workers anyOne process.
	pool := nil.
	self garbageCollectAndWait.
	self assert: workerProcess first isNil
]

{ #category : #tests }
TKTCommonQueueWorkerPoolTests >> testWorkerPoolWorkersProcessesAreStoppedAfterPoolsCollection [
	<expectedFailure>
	| pool workerProcess |
	pool := TKTCommonQueueWorkerPool new.
	pool name: 'Test pool' , UUID new asString.
	pool poolMaxSize: 4.
	pool start.
	(pool future: (MessageSend receiver: self selector: #yourself))
		waitForCompletion: 1 second.
	workerProcess := WeakArray with: pool workers anyOne process process.
	pool := nil.
	self garbageCollectAndWait.
	self assert: workerProcess first isNil
]

{ #category : #tests }
TKTCommonQueueWorkerPoolTests >> testWorkerPoolWorkersProcessesAreTerminatedAfterPoolsCollection [
	| pool workerProcess |
	pool := TKTCommonQueueWorkerPool createDefault.
	(pool future: (MessageSend receiver: self selector: #yourself)) waitForCompletion: 1 second.
	workerProcess := pool workers anyOne process process.
	pool := nil.
	self garbageCollectAndWait.
	workerProcess isTerminated.
	self assert: workerProcess isTerminated
]
