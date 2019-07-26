"
I am the unique object for the current smalltalk session (I.e. between a startup and a shutdown).
On each image startup the current session is invalidated and a new session is created.
I define how errors should be handled.

ps: I replace the old Session class that was only used as a session identifier  
"
Class {
	#name : #WorkingSession,
	#superclass : #Object,
	#instVars : [
		'manager',
		'deferredStartupActions',
		'id',
		'creationTime',
		'properties'
	],
	#category : #'System-SessionManager-Utilities'
}

{ #category : #accessing }
WorkingSession >> accessMode [

	^ self properties at: #accessMode ifAbsent: [ #readWrite ]
]

{ #category : #accessing }
WorkingSession >> accessMode: anObject [

	^ self properties at: #accessMode put: anObject
]

{ #category : #'deferred startup actions' }
WorkingSession >> addDeferredStartupAction: aBlock [
	"Add the block to the list of actions that we will be performed immediately after the startup list is executed."
	deferredStartupActions add: aBlock
]

{ #category : #accessing }
WorkingSession >> creationTime [
	^ creationTime
]

{ #category : #accessing }
WorkingSession >> errorHandler [
	^ UIManager default
]

{ #category : #'deferred startup actions' }
WorkingSession >> executeDeferredStartupActions: resuming [
	"Execute the deferred actions which where added during startup, 
	resuming is true if the image was started fresh, false if we just saved an image"

	[ deferredStartupActions notEmpty ] 
		whileTrue: [ | action |
			action := deferredStartupActions removeFirst.
			action cull: resuming
			"We do not handle errors on deferred startup actions as there is already the environment ready to use.
			The current UI manager will handle the error." ]
]

{ #category : #accessing }
WorkingSession >> id [
	^ id ifNil: [ id := UUID new ]
]

{ #category : #initialization }
WorkingSession >> initialize [
	super initialize.
	deferredStartupActions := OrderedCollection new.
	creationTime := DateAndTime now.
	properties := Dictionary new.
]

{ #category : #'startup - shutdown' }
WorkingSession >> install [
	manager installSession: self
]

{ #category : #testing }
WorkingSession >> isDisabledAccessMode [

	^ self accessMode = #disabled
]

{ #category : #testing }
WorkingSession >> isReadOnlyAccessMode [

	^ self accessMode = #readOnly
]

{ #category : #testing }
WorkingSession >> isReadWriteAccessMode [

	^ self accessMode = #readWrite
]

{ #category : #testing }
WorkingSession >> isWriteOnlyAccessMode [

	^ self accessMode = #writeOnly
]

{ #category : #accessing }
WorkingSession >> manager: aSessionManager [ 
	manager := aSessionManager
]

{ #category : #'startup - shutdown' }
WorkingSession >> properties [

	^ properties
]

{ #category : #'startup - shutdown' }
WorkingSession >> runList: aList do: aBlock [
	aList do: [ :each | 
		[aBlock value: each] 
			on: Exception
			do: [ :error | self errorHandler handleError: error] ]
]

{ #category : #'startup - shutdown' }
WorkingSession >> runShutdown: isImageQuitting [
	self
		runList: manager shutdownList
		do: [ :each | each shutdown: isImageQuitting ]
]

{ #category : #'startup - shutdown' }
WorkingSession >> runStartup: isImageStarting [
	self
		runList: manager startupList
		do: [ :each | each startup: isImageStarting ].
	
]

{ #category : #'startup - shutdown' }
WorkingSession >> start: isImageStarting [
	Smalltalk vm setGCParameters.
	isImageStarting
		ifTrue: [ Smalltalk clearExternalObjects ].
	
	self runStartup: isImageStarting.
]

{ #category : #'startup - shutdown' }
WorkingSession >> stop: isImageQuitting [

	Object flushDependents.
	self runShutdown: isImageQuitting.
]

{ #category : #'startup - shutdown' }
WorkingSession >> uninstall [
	manager uninstallSession: self
]
