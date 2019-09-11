"
This is a utility that: 
- executes a job asynchroniously
- it remembers the process so that it can terminate the job, if needed
- triggers a callback when it's done
"
Class {
	#name : #GLMAsyncTask,
	#superclass : #Object,
	#instVars : [
		'process',
		'callback',
		'delay',
		'task',
		'priority',
		'arguments'
	],
	#category : #'Glamour-Tasks'
}

{ #category : #accessing }
GLMAsyncTask >> arguments [

	^ arguments ifNil: [ arguments := {  } ]
]

{ #category : #scripting }
GLMAsyncTask >> arguments: aCollection [

	arguments := aCollection
]

{ #category : #accessing }
GLMAsyncTask >> callback [

	^ callback ifNil: [ callback := [  ] ]
]

{ #category : #async }
GLMAsyncTask >> complete: anObject callback: aBlock [
	
	process ifNil: [ ^ self terminate ].
	process isTerminated ifFalse: [ "for some cases when we try to return result of wrong process" ^ self ].
	
	callback = aBlock ifFalse: [ ^ self terminate ].
	
	callback ifNotNil: [ callback cull: anObject ].
	process := nil
]

{ #category : #accessing }
GLMAsyncTask >> delay [

	^ delay ifNil: [ delay := 0 ]
]

{ #category : #scripting }
GLMAsyncTask >> delay: anInteger [

	delay := anInteger
]

{ #category : #scripting }
GLMAsyncTask >> doInBackground: aBlock [

	task := aBlock
]

{ #category : #scripting }
GLMAsyncTask >> execute [

	^ self execute: self arguments
]

{ #category : #scripting }
GLMAsyncTask >> execute: anArguments [

	^ self
		execute: self task
		withArguments: anArguments
		callback: self callback
		priority: self priority
		delay: self delay
]

{ #category : #async }
GLMAsyncTask >> execute: aBlock withArguments: aCollection callback: callbackBlock [
	"execution happens with UI process priority so that communication with UI can be done in real time"
	
	^ self
		execute: aBlock
		withArguments: aCollection
		callback: callbackBlock
		priority: Processor userSchedulingPriority
]

{ #category : #async }
GLMAsyncTask >> execute: aBlock withArguments: aCollection callback: callbackBlock delay: anIntegerMilliseconds [
	"execution happens with UI process priority so that communication with UI can be done in real time"
	
	^ self
		execute: aBlock
		withArguments: aCollection
		callback: callbackBlock
		priority: Processor userSchedulingPriority
		delay: anIntegerMilliseconds
]

{ #category : #async }
GLMAsyncTask >> execute: aBlock withArguments: aCollection callback: callbackBlock priority: aPriority [
		
	^ self
		execute: aBlock
		withArguments: aCollection
		callback: callbackBlock
		priority: aPriority
		delay: 0
]

{ #category : #async }
GLMAsyncTask >> execute: aBlock withArguments: aCollection callback: callbackBlock priority: aPriority delay: anIntegerMilliseconds [
	
	self terminate.
	
	callback := callbackBlock.
	process := [ |result|
		anIntegerMilliseconds > 0 ifTrue: [ anIntegerMilliseconds milliSeconds wait ].
		result := aBlock valueWithEnoughArguments: aCollection.
		"we use it to pass result from background thread and execute callback block in UI thread"
		UIManager default defer: [ self complete: result callback: callbackBlock ]
	]	newProcess.
	
	process priority: aPriority.
	process resume.
		
	^ self
]

{ #category : #accessing }
GLMAsyncTask >> isTerminated [

	process ifNil: [ ^ true ].
	^ process isTerminated
]

{ #category : #scripting }
GLMAsyncTask >> onCompleted: aBlock [

	callback := aBlock
]

{ #category : #async }
GLMAsyncTask >> pause [

	process ifNotNil: [ process suspend ]
]

{ #category : #async }
GLMAsyncTask >> perform: aBlock delay: anInteger [

	self
		execute: []
		withArguments: {}
		callback: aBlock
		delay: anInteger
]

{ #category : #async }
GLMAsyncTask >> perform: aBlock delay: anInteger priority: aPriority [

	self
		execute: []
		withArguments: {}
		callback: aBlock
		priority: aPriority
		delay: anInteger
]

{ #category : #accessing }
GLMAsyncTask >> priority [

	^ priority ifNil: [ priority := Processor userSchedulingPriority ]
]

{ #category : #scripting }
GLMAsyncTask >> priority: anInteger [

	priority := anInteger
]

{ #category : #async }
GLMAsyncTask >> resume [

	process ifNotNil: [ process resume ]
]

{ #category : #accessing }
GLMAsyncTask >> task [

	^ task ifNil: [ task := [  ] ]
]

{ #category : #async }
GLMAsyncTask >> terminate [

	process ifNotNil: [ process terminate. process := nil ]
]
