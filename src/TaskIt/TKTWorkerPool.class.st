"
! TKTWorkerPool

A TaskIT worker pool is pool of worker runners, equivalent to a ThreadPool from other programming languages. Its main purpose is to provide several worker runners and decouple us from the management of threads/processes. A worker pool is a runner in the sense we use the schedule: message to schedule tasks in it. Internally, all runners inside a worker pool share a single task queue.

Different applications may have different concurrency needs, thus, TaskIT worker pools do not provide a default amount of workers. Before using a pool, we need to specify the maximum number of workers in the pool using the poolMaxSize: message. A worker pool will create new workers on demand.

[[[language=smalltalk
pool := TKTWorkerPool new.
pool poolMaxSize: 5.
]]]

TaskIT worker pools use internally an extra worker to synchronize the access to its task queue. Because of this, a worker pool has to be manually started using the start message before scheduled messages start to be executed.

[[[language=smalltalk
pool := TKTWorkerPool new.
pool poolMaxSize: 5.
pool start.
pool schedule: [ 1 logCr ].
]]]

Once we are done with the worker pool, we can stop it by sending it the stop message.

[[[language=smalltalk
pool stop.
]]]
"
Class {
	#name : #TKTWorkerPool,
	#superclass : #Object,
	#traits : 'TTaskScheduler - {#scheduleTaskExecution:}',
	#classTraits : 'TTaskScheduler classTrait',
	#instVars : [
		'poolMaxSize',
		'busyWorkers',
		'freeWorkers',
		'poolWorker',
		'workers',
		'priority'
	],
	#category : #'TaskIt-Worker'
}

{ #category : #schedulling }
TKTWorkerPool >> getAvailableWorker [

	| worker |
	worker := freeWorkers nextIfNone: [
			self size < poolMaxSize
				ifTrue: [ self newWorker ]
				ifFalse: [ freeWorkers next ] ].
	^ worker
]

{ #category : #initialization }
TKTWorkerPool >> initialize [

	super initialize.

	workers := Set new.
	freeWorkers := AtomicSharedQueue new.

	poolWorker := TKTWorker new.
	self name: 'Worker' , self identityHash asString.
]

{ #category : #testing }
TKTWorkerPool >> isFree [

	^ workers allSatisfy: [ :each | each isFree ]
]

{ #category : #accessing }
TKTWorkerPool >> name [
	
	^ poolWorker name
]

{ #category : #accessing }
TKTWorkerPool >> name: aString [ 
	
	poolWorker name: aString
]

{ #category : #schedulling }
TKTWorkerPool >> newWorker [	
	| newWorker |
	newWorker := TKTWorker new.
	newWorker
		priority: self priority;
		name: self name, ' Worker #', (self size + 1) asString;
		start.
	^ workers add: newWorker
]

{ #category : #accessing }
TKTWorkerPool >> poolMaxSize: anInteger [ 
	
	poolMaxSize := anInteger
]

{ #category : #printing }
TKTWorkerPool >> printOn: aStream [

	aStream
		nextPutAll: 'TKTWorkerPool(';
		nextPutAll: 'poolMaxSize: ';
		print: poolMaxSize;
		nextPutAll: '; busyWorkers: ';
		print: workers size - freeWorkers size;
		nextPutAll: '; freeWorkers: ';
		print: freeWorkers size;
		nextPutAll: ')'
]

{ #category : #accessing }
TKTWorkerPool >> priority [
	^priority
]

{ #category : #accessing }
TKTWorkerPool >> priority: anInteger [ 
	priority := anInteger.
	workers do: [ : each | each priority: anInteger ]
]

{ #category : #schedulling }
TKTWorkerPool >> purge [

	poolWorker purge
]

{ #category : #schedulling }
TKTWorkerPool >> returnWorker: aTKTWorker [ 

	freeWorkers nextPut: aTKTWorker
]

{ #category : #schedulling }
TKTWorkerPool >> scheduleTaskExecution: aTaskExecution [
	
	poolWorker schedule: [ | worker |
		worker := self getAvailableWorker.
		worker schedule: [
			aTaskExecution value.
			self returnWorker: worker ] ]
]

{ #category : #accessing }
TKTWorkerPool >> size [
	
	^ workers size
]

{ #category : #starting }
TKTWorkerPool >> start [ 
	poolWorker start
]

{ #category : #starting }
TKTWorkerPool >> stop [

	poolWorker stop.
	
	workers do: #stop.
	workers removeAll.
	
	"Empty the free workers queue"
	[freeWorkers isEmpty]
		whileFalse: [ freeWorkers next ]
]

{ #category : #accessing }
TKTWorkerPool >> workers [
	
	^ workers copy
]
