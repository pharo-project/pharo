"
! TKTWorker

The worker runner, instance of TKTWorker, is a task runner that uses a single process to execute tasks from a queue. The worker's single process removes one-by-one the tasks from the queue and executes them sequenceally. Then, schedulling a task into a worker means to add the task inside the queue.

A worker manages the life-cycle of its process and provides the messages start and stop to control when the worker thread will begin and end.

[[[language=smalltalk
worker := TKTWorker new.
worker start.
worker schedule: [ 1 + 5 ].
worker stop.
]]]

By using workers, we can control the amount of alive processes and how tasks are distributed amongst them. For example, in the following example three tasks are executed sequenceally in a single separate process while still allowing us to use an asynchronous style of programming.

[[[language=smalltalk
worker := TKTWorker new start.
future1 := worker future: [ 2 + 2 ].
future2 := worker future: [ 3 + 3 ].
future3 := worker future: [ 1 + 1 ].
]]]

Workers can be combined into worker pools.
"
Class {
	#name : #TKTWorker,
	#superclass : #TKTQueueTaskScheduler,
	#traits : 'TFinalizable',
	#classTraits : 'TFinalizable classTrait',
	#instVars : [
		'process',
		'name',
		'exceptionHandler',
		'priority'
	],
	#category : #'TaskIt-Worker'
}

{ #category : #current }
TKTWorker class >> createDefault [
	^ self new
		queue: AtomicSharedQueue new;
		yourself
]

{ #category : #'start-stop' }
TKTWorker >> currentTaskExecution [
	^ process ifNotNil: [ process currentTaskExecution ] ifNil: [ nil ]
]

{ #category : #initialization }
TKTWorker >> ensureIsBeingWatched [
	TKTConfiguration watchDog ensureIsWatching: self
]

{ #category : #starting }
TKTWorker >> ensureIsWorking [
	self isRunning
		ifFalse: [ self restart ]
]

{ #category : #accessing }
TKTWorker >> exceptionHandler [
	
	^ process exceptionHandler
]

{ #category : #accessing }
TKTWorker >> exceptionHandler: aHandler [
	exceptionHandler := aHandler.
	process ifNotNil: [ process exceptionHandler: aHandler ]
]

{ #category : #finalization }
TKTWorker >> executor [

	"The finalization scheme runs in another object (the executor) that does not have 
	all the state, if it has all the state a loop is generated and never GCed"

	^ TKTWorkerExecutor new
		processToKill: process;
		yourself.
]

{ #category : #initialization }
TKTWorker >> initialize [
	super initialize.
	self name: ('Worker-{ID}' format: {(#ID -> UUID new)} asDictionary).
	self ensureIsBeingWatched
	
]

{ #category : #testing }
TKTWorker >> isBusy [
	^ self isFree not
]

{ #category : #testing }
TKTWorker >> isFree [
	^ process ifNil: [ true ] ifNotNil: [ process isFree ]
]

{ #category : #testing }
TKTWorker >> isRunning [
	^ process isNotNil and: [ process isRunning ]
]

{ #category : #schedulling }
TKTWorker >> isWorker [
	^ true
]

{ #category : #accessing }
TKTWorker >> name [
	^ process ifNil: [ name ] ifNotNil: [ process name ]
]

{ #category : #accessing }
TKTWorker >> name: aString [
	name := aString.
	process ifNotNil: [ process name: aString ]
]

{ #category : #accessing }
TKTWorker >> priority [
	^ priority
]

{ #category : #accessing }
TKTWorker >> priority: anInteger [
	priority := anInteger.
	self isRunning ifTrue: [self process priority: anInteger]
]

{ #category : #starting }
TKTWorker >> privatePrepareProcess: aWorkerProcess [
	process := aWorkerProcess.
	process
		taskQueue: queue;
		name: name;
		yourself.
	exceptionHandler
		ifNotNil: [ process exceptionHandler: exceptionHandler ].
	self noteNeedsToBeFinalized
]

{ #category : #starting }
TKTWorker >> privateStart [
	process start
]

{ #category : #accessing }
TKTWorker >> process [

	^ process
]

{ #category : #accessing }
TKTWorker >> queue [
	^ queue
]

{ #category : #starting }
TKTWorker >> restart [
	self stop.
	self start.
]

{ #category : #starting }
TKTWorker >> start [
	self privatePrepareProcess: TKTConfiguration poolWorkerProcess new.
	self privateStart.
	self priority ifNotNil: [self process priority: self priority].

]

{ #category : #starting }
TKTWorker >> stop [
	process
		ifNotNil: [ process stop.
			process := nil ].
	TKTConfiguration watchDog stopWatching: self.
	self noteDoesNotNeedToBeFinalized.
]

{ #category : #accessing }
TKTWorker >> taskQueue: anAtomicSharedQueue [
	queue := anAtomicSharedQueue.
	process ifNotNil: [ process taskQueue: queue ]
]
