"
Watchdog watches the common queue worker pool to ensure the good working of this pool
"
Class {
	#name : #TKTWatchDog,
	#superclass : #TKTService,
	#instVars : [
		'values',
		'id',
		'started'
	],
	#classInstVars : [
		'onDuty'
	],
	#category : #'TaskIt-Worker'
}

{ #category : #accessing }
TKTWatchDog class >> onDuty [
	^ onDuty
		ifNil: [ onDuty := self new.
			onDuty start.
			onDuty ]
]

{ #category : #reseting }
TKTWatchDog class >> reset [
	
	onDuty ifNotNil: [ onDuty stop ].
	onDuty := nil.
]

{ #category : #initialization }
TKTWatchDog >> cleanUpImageToStart [
	values removeAll
]

{ #category : #initialization }
TKTWatchDog >> delay [
	^ 10 seconds
]

{ #category : #initialization }
TKTWatchDog >> ensureIsAwake [
	self isRunning
		ifFalse: [ self restart ]
]

{ #category : #initialization }
TKTWatchDog >> ensureIsWatching: aValue [
	worker = aValue ifTrue: [ ^ self ].
	(values includes: aValue)
		ifFalse: [ self watch: aValue ].
	self ensureIsAwake
]

{ #category : #initialization }
TKTWatchDog >> initialize [
	super initialize.
	values := WeakSet new.
	id := UUID new.
	started := false
]

{ #category : #initialization }
TKTWatchDog >> name [
	^ 'WatchDog (' , id asString , ')'
]

{ #category : #accessing }
TKTWatchDog >> pharoProcess [
	^ worker process process
]

{ #category : #initialization }
TKTWatchDog >> privateStart [
	stopRequested := false.
	stopCallbacks := Set new.
	" A worker normally needs to be watched. But the watchdog process cannot be watched, because the watchdog do not yet exists. 
	 This is a hacky workaround. Think a better solution.
	"
	worker := TKTWorker basicNew.
	worker initialize.
	worker taskQueue: AtomicSharedQueue new.
	recursiveTask := [ self iterateService.
	self scheduleRecursiveTask ] asTask.
	TKTConfiguration serviceManager startService: self
]

{ #category : #initialization }
TKTWatchDog >> start [
	started
		ifFalse: [ started := true.
			self privateStart ]
]

{ #category : #initialization }
TKTWatchDog >> stepService [
	self watchedValues
		ifEmpty: [ self stop ]
		ifNotEmpty: [ values do: [ :v | self watchDog: v ].
			self delay wait ]
]

{ #category : #initialization }
TKTWatchDog >> stop [
	started := false.
	super stop
]

{ #category : #initialization }
TKTWatchDog >> stopWatching: aValue [
	values remove: aValue ifAbsent: [  ]
]

{ #category : #initialization }
TKTWatchDog >> watch: aValue [
	values add: aValue
]

{ #category : #initialization }
TKTWatchDog >> watchDog: aParameterPool [
	[ aParameterPool ensureIsWorking ]
		on: UnhandledError 
		do: [ :err | 
			self traceCr: err printString.
			self
				log:
					'Watch dog is not watching anymore the value: '
						,
							(values remove: aParameterPool ifAbsent: [ aParameterPool ])
								printString ]
]

{ #category : #initialization }
TKTWatchDog >> watchedValues [
	
	^  values
]
