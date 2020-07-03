"
A service is a process that executes a task over and over again. You can think about a web server, or a database server that needs to be up and running and listening to new connections all the time.

Each TaskIt service may define a `setUp`, a `tearDown` and a `stepService`. `setUp` is run when a service is being started, `shutDown` is run when the service is being shutted down, and `stepService` is the main service action that will be executed repeateadly.

Creating a new service is as easy as creating a subclass of `TKTService`. For example, let's create a service that watches the existence of a file. If the file does not exists it will log it to the transcript. It will also log when the service starts and stops to the transcript.

```smalltalk
TKTService subclass: #TKTFileWatcher
  instanceVariableNames: 'file'
  classVariableNames: ''
  package: 'TaskItServices-Tests'
```

Hooking on the service's `setUp` and `tearDown` is as easy as overriding such methods:

```smalltalk
TKTFileWatcher >> setUp
  super setUp.
  Transcript show: 'File watcher started'.

TKTFileWatcher >> tearDown
  super tearDown.
  Transcript show: 'File watcher finished'.
```

Finally, setting the watcher action is as easy as overriding the `stepService` message.

```smalltalk
TKTFileWatcher >> stepService
  1 second wait.
  file asFileReference exists
    ifFalse: [ Transcript show: 'file does not exist!' ]
```

Making the service work requires yet an additional method: the service name. Each service should provide a unique name through the `name` method. TaskIt verifies that service names are unique and prevents the starting of two services with the same name.

```smalltalk
TKTFileWatcher >> name
  ^ 'Watcher file: ', file asString
```

Once your service is finished, starting it is as easy as sending it the `start` message.

```smalltalk
watcher := TKTFileWatcher new.
watcher file: 'temp.txt'.
watcher start.
```

Requesting the stop of a service is done by sending it the `stop` message. Know that sending the `stop` message will not stop the service right away. It will actually request it to stop, which will schedule the tear down of the service and kill its process after that. 

```smalltalk
watcher stop.
```

Stopping the process in an unsafe way is also supported by sending it the `kill` message. Killing a service will stop it right away, interrupting whatever task it was executing.

```smalltalk
watcher kill.
```
"
Class {
	#name : #TKTService,
	#superclass : #Object,
	#instVars : [
		'worker',
		'stopRequested',
		'stopCallbacks',
		'stepDelay',
		'recursiveTask',
		'currentDelay'
	],
	#category : #'TaskIt-Services'
}

{ #category : #starting }
TKTService >> doStop [
	self tearDown.
	(TKTNewProcessTaskRunner new 
		future: [ self kill ]) 
		flatCollect: [ :r | 
			stopCallbacks do: [ :each | each value: r ] ]
]

{ #category : #initialization }
TKTService >> initialize [
	super initialize.
	self stepDelay: 500 milliSeconds.

]

{ #category : #testing }
TKTService >> isRunning [
	worker ifNil: [ ^ false ].
	^ worker isRunning
]

{ #category : #stepping }
TKTService >> iterateService [
	self stepService.
	currentDelay := stepDelay asDelay.
	currentDelay wait
]

{ #category : #private }
TKTService >> kill [
	TKTConfiguration serviceManager removeService: self.
	worker stop
]

{ #category : #private }
TKTService >> launch [

	self name ifNil: [ self error: 'A name should be provided for a service' ].
	
	worker name: self name.
	worker start.
	worker schedule: [ self setUp ].
	self scheduleRecursiveTask.
]

{ #category : #accessing }
TKTService >> name [
	
	self subclassResponsibility
]

{ #category : #starting }
TKTService >> onStoppedDo: aCallback [

	stopCallbacks add: aCallback
]

{ #category : #accessing }
TKTService >> priority [
	^ self worker priority
]

{ #category : #accessing }
TKTService >> priority: anInteger [ 
	self worker priority: anInteger
]

{ #category : #accessing }
TKTService >> process [
	^worker process
]

{ #category : #starting }
TKTService >> restart [
	self stop.
	0.5 second wait.
	stopRequested := false.
	stopCallbacks := Set new.
	self start
]

{ #category : #starting }
TKTService >> scheduleRecursiveTask [
	stopRequested
		ifTrue: [ worker schedule: [ self doStop ] ]
		ifFalse: [ worker schedule: recursiveTask ]
]

{ #category : #stepping }
TKTService >> setUp [

	"Hook"
]

{ #category : #starting }
TKTService >> start [
	stopRequested := false.
	stopCallbacks := Set new.
	self worker taskQueue: AtomicSharedQueue new.
	recursiveTask := [ 
			self iterateService.
			self scheduleRecursiveTask ] asTask.
	TKTConfiguration serviceManager startService: self
]

{ #category : #accessing }
TKTService >> stepDelay [
	^ stepDelay 
]

{ #category : #starting }
TKTService >> stepDelay: aTimeDelay [
	stepDelay := aTimeDelay 
]

{ #category : #stepping }
TKTService >> stepService [

	self subclassResponsibility
]

{ #category : #starting }
TKTService >> stop [
	| futureStop |
	futureStop := TKTFuture new.
	self onStoppedDo: [ :v | futureStop deploySuccess: self ].
	stopRequested := true.
	TKTConfiguration serviceManager removeService: self.
	currentDelay ifNotNil: [:delay | delay delaySemaphore signal ].
	^ futureStop
]

{ #category : #stepping }
TKTService >> tearDown [

	"Hook"
]

{ #category : #accessing }
TKTService >> worker [
	^ worker ifNil: [ worker :=  TKTWorker new]
]
