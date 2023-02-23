"
This worker executes the function within the worker defined in a `ProcessLocalVariable`, allowing the user to execute the same function in two different native threads (this is very useful for executing functions that wait for some signal to return, e.g. reading from a socket or a piped file descriptor).

It is important to define a `TFProcessLocalWorker>>defaultWorker` (even if by default it will take the `TFSameThreadRunner`.

Example: 

You define it like this: 
```
MyLibrary>>runner 

	^ TFProcessLocalWorker new
		defaultWorker: super runner;
		yourself
```

Then you can execute a function like this: 
```
worker := TFWorker named: 'QUEUE'.
[
	TFProcessLocalWorker worker: worker.
	channel readLineFromSocketInSeparatedWorker.
]

"
Class {
	#name : #TFProcessLocalWorker,
	#superclass : #TFRunner,
	#instVars : [
		'defaultWorker'
	],
	#classInstVars : [
		'worker'
	],
	#category : #'ThreadedFFI-Worker'
}

{ #category : #'class initialization' }
TFProcessLocalWorker class >> initialize [

	worker := ProcessLocalVariable new
]

{ #category : #accessing }
TFProcessLocalWorker class >> worker [

	^ worker value
]

{ #category : #accessing }
TFProcessLocalWorker class >> worker: aWorker [

	worker value: aWorker
]

{ #category : #accessing }
TFProcessLocalWorker >> defaultWorker [

	^ defaultWorker ifNil: [ TFSameThreadRunner uniqueInstance ]
]

{ #category : #accessing }
TFProcessLocalWorker >> defaultWorker: aWorker [

	defaultWorker := aWorker
]

{ #category : #executing }
TFProcessLocalWorker >> ensureInitialized [

	self worker ensureInitialized
]

{ #category : #accessing }
TFProcessLocalWorker >> forCallback [

	^ self worker
]

{ #category : #executing }
TFProcessLocalWorker >> invokeFunction: aTFExternalFunction withArguments: aCollection [

	^ self worker
		invokeFunction: aTFExternalFunction
		withArguments: aCollection
]

{ #category : #private }
TFProcessLocalWorker >> primitivePerformWorkerCall: aTFExternalFunction
		withArguments: argumentHolder
		withReturnHolder: aReturnHolder
		usingSemaphore: anInteger [

	self error: 'Should not arrive here'
]

{ #category : #executing }
TFProcessLocalWorker >> worker [

	^ self class worker ifNil: [ self defaultWorker ]
]
