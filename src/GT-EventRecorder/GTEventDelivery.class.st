"
I know how to deliver usage and diagnostic data to a remote server.

I start and maintain a process for sending data on a regular inteval (#standardDuration).
I maintain a queue with the data that needs to be send. If a delivert fails I re-add the data to the queue.

Data that is added to my queue must be packaged in an object of type GTEventBundle.  I use the url specified by the bundle.
I send the recorder the message #pack to notify it that I enter the delivery cycle.

Public API and Key Messages

- #activate initializes the delivery process; clients must call this method; it's not called automatically on object creation. 
- #deliveryCycle implements  the main logic of the delivery process
"
Class {
	#name : #GTEventDelivery,
	#superclass : #Object,
	#instVars : [
		'mutex',
		'process',
		'delay',
		'permission',
		'recorder',
		'queue',
		'preSuspendingAction'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #activation }
GTEventDelivery >> activate [
	"Initialize a delivery process."
	^ mutex critical: [ 
			self offerPermission.
			self isInactive ifTrue: [ self initializeProcess ].
			process ]
]

{ #category : #adding }
GTEventDelivery >> add: aGTEventBundle [
	queue nextPut: aGTEventBundle
]

{ #category : #announce }
GTEventDelivery >> announceSuspendingDelivery [
	preSuspendingAction ifNotNil: #value
]

{ #category : #accessing }
GTEventDelivery >> announcer [
	^ self recorder ifNotNil: #announcer
]

{ #category : #delivery }
GTEventDelivery >> bundles [
	"Be careful about using this method. You will loose data if you call it externaly."
	| eventBundles |
	eventBundles := OrderedCollection new.
	queue flush: [ :eachGTEventBundle | eventBundles addLast: eachGTEventBundle ].
	^ eventBundles
]

{ #category : #deactivation }
GTEventDelivery >> deactivate [
	^ mutex critical: [ | oldProcess |
		self withdrawPermission.
		delay ifNotNil: [ :aDelay | aDelay delaySemaphore signal ].
		oldProcess := process.
		process := nil.
		oldProcess ]
]

{ #category : #delivery }
GTEventDelivery >> deliver: aBoolean [
	| bundles |
	bundles := GTEventMultiBundleFactory fromBundles: self bundles.
	bundles do: [ :eachGTEventMultiBundle |
		self deliver: eachGTEventMultiBundle signalProgress: aBoolean ]
]

{ #category : #delivery }
GTEventDelivery >> deliver: aGTEventMultiBundle signalProgress: aBoolean [
	^ (ZnClient new
			beOneShot;
			timeout: 5;
			signalProgress: aBoolean;
			numberOfRetries: 0;
			url: aGTEventMultiBundle url;
			enforceHttpSuccess: true;
			ifFail: [ :exception | queue nextPut: aGTEventMultiBundle ];
			entity: aGTEventMultiBundle entity;
			put;
			response)
]

{ #category : #initialization }
GTEventDelivery >> deliveryCycle [
	delay := self standardDuration asDelay.
	self announceSuspendingDelivery.
	delay wait.
	delay := nil.
	self hasPermission ifTrue: [ self packAndDeliver: false ]
]

{ #category : #testing }
GTEventDelivery >> hasPermission [
	^ permission
]

{ #category : #initialization }
GTEventDelivery >> initialize [
	super initialize.
	mutex := Mutex new.
	queue := WaitfreeQueue new.
	permission := true.
	preSuspendingAction := nil.
]

{ #category : #initialization }
GTEventDelivery >> initializeProcess [
	process := [ [ self hasPermission ] whileTrue: [ self deliveryCycle ] ]
		forkAt: Processor userBackgroundPriority
		named: self printProcessName
]

{ #category : #testing }
GTEventDelivery >> isActive [
	^ self isInactive not
]

{ #category : #testing }
GTEventDelivery >> isGlobalRecorder [
	^ self recorder 
			ifNil: false
			ifNotNil: #isGlobalRecorder
]

{ #category : #testing }
GTEventDelivery >> isInactive [
	^ mutex critical: [ 
		process isNil or: [
			process isTerminated or: [
				process isTerminating ] ] ]
]

{ #category : #delivery }
GTEventDelivery >> now [
	"for testing purpose"
	delay ifNotNil: [ :aDelay | aDelay delaySemaphore signal. aDelay unschedule. ]
]

{ #category : #activation }
GTEventDelivery >> offerPermission [
	mutex critical: [ permission := true ]
]

{ #category : #delivery }
GTEventDelivery >> packAndDeliver: aBoolean. [
	recorder pack.
	self deliver: aBoolean.
	
]

{ #category : #initialization }
GTEventDelivery >> preSuspendingAction: aBlockClosure [ 
	preSuspendingAction := aBlockClosure
]

{ #category : #printing }
GTEventDelivery >> printOn: aStream [
	super printOn: aStream.
	aStream
		space;
		nextPut: $[;
		print: queue size;
		space;
		nextPutAll: ('item' asPluralBasedOn: queue size).
	self isActive
		ifTrue: [ 
			delay 
				ifNil: [ aStream nextPutAll: 'process active, but delay is nil!' ]
				ifNotNil: [ :aDelay |
					aStream
						nextPutAll: ', next cycle in ';
						print: (aDelay millisecondsToGo milliSeconds roundTo: 1 second);
						nextPutAll: ' sec' ] ]
		ifFalse: [ 
			aStream nextPutAll: ', inactive' ].
	aStream		
		nextPut: $]
]

{ #category : #printing }
GTEventDelivery >> printProcessName [
	^ String streamContents: [ :aStream |
		self printProcessNameOn: aStream ]
]

{ #category : #printing }
GTEventDelivery >> printProcessNameOn: aStream [
	aStream
		nextPutAll: self className.
	self isGlobalRecorder
		ifTrue: [ aStream nextPutAll: ' global ' ]
		ifFalse: [ aStream nextPutAll: ' a ' ].
	aStream nextPutAll: 'delivery process'
]

{ #category : #accessing }
GTEventDelivery >> recorder [
	^ recorder
]

{ #category : #accessing }
GTEventDelivery >> recorder: anObject [
	recorder := anObject
]

{ #category : #accessing }
GTEventDelivery >> size [
	^ queue size
]

{ #category : #accessing }
GTEventDelivery >> standardDuration [
	^ 20 minutes
]

{ #category : #deactivation }
GTEventDelivery >> withdrawPermission [
	mutex critical: [ permission := false ]
]
