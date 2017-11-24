"
I schedule and postpone a message send until the specified duration passed.
"
Class {
	#name : #OmDeferrer,
	#superclass : #Object,
	#instVars : [
		'process',
		'lastSchedule',
		'duration',
		'selector',
		'object'
	],
	#category : #'Ombu-Common'
}

{ #category : #initialization }
OmDeferrer class >> initialize [
	"
	self initialize
	"
	SessionManager default registerUserClassNamed: self name

]

{ #category : #'instance creation' }
OmDeferrer class >> new [

	^ self shouldNotImplement
]

{ #category : #'instance creation' }
OmDeferrer class >> send: selector to: object after: aDuration [

	^ self basicNew
		initializeWithSelector: selector
		object: object
		duration: aDuration
]

{ #category : #'system startup' }
OmDeferrer class >> shutDown: quitting [
	"Force my instances to flush before image quits"

	self allInstancesDo: [:each | each flush ].
]

{ #category : #accessing }
OmDeferrer >> duration [
	^ duration
]

{ #category : #accessing }
OmDeferrer >> duration: aDuration [

	duration := aDuration
]

{ #category : #scheduling }
OmDeferrer >> flush [

	process ifNotNil: [
		"Execute deferred action and clean up"
		[ self sendMessage ] ensure: [ process := nil ] 
	].
]

{ #category : #initialization }
OmDeferrer >> initializeWithSelector: aSelector object: anObject duration: aDuration [ 
	
	self initialize.
	selector := aSelector.
	object := anObject.
	duration := aDuration.
]

{ #category : #accessing }
OmDeferrer >> object [
	^ object
]

{ #category : #accessing }
OmDeferrer >> object: anObject [
	object := anObject
]

{ #category : #scheduling }
OmDeferrer >> runMessageProcess [
	process := [ 
		"Loop until enough time passed since last #schedule"
		[ DateAndTime now < (lastSchedule + duration) ]
			whileTrue: [ duration wait ].
			
		"Execute deferred action (if not flushed before) and clean up"
		self flush.
	] fork
]

{ #category : #scheduling }
OmDeferrer >> schedule [
	lastSchedule := DateAndTime now.

	process ifNil: [ 
		"Deferrer can be activated during tests which intercept any forked processes.
		To prevent it we should ensure here that message process is running out of tests environment"
		DefaultExecutionEnvironment beActiveDuring: [ self runMessageProcess ]]
]

{ #category : #accessing }
OmDeferrer >> selector [
	^ selector
]

{ #category : #accessing }
OmDeferrer >> selector: anObject [
	selector := anObject
]

{ #category : #private }
OmDeferrer >> sendMessage [
	
	object perform: selector
]
