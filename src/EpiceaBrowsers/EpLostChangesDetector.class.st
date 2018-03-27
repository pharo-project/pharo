"
I check on image startup that there is lost logs for current image and I open browser on it to show changes to user.
I super usefull for recovering lost changes after image crash
"
Class {
	#name : #EpLostChangesDetector,
	#superclass : #Object,
	#instVars : [
		'hasLostChanges',
		'lastSessionFileStore'
	],
	#classInstVars : [
		'enabled'
	],
	#category : #'EpiceaBrowsers-Integration'
}

{ #category : #accessing }
EpLostChangesDetector class >> enabled: aBoolean [

	enabled := aBoolean
]

{ #category : #initialization }
EpLostChangesDetector class >> initialize [
	"
	self initialize
	"
	SessionManager default registerToolClassNamed: self name

]

{ #category : #testing }
EpLostChangesDetector class >> isEnabled [

	^ enabled ifNil: [ enabled := true ]
]

{ #category : #'instance creation' }
EpLostChangesDetector class >> new [

	^ self newWithLog: EpMonitor current log
]

{ #category : #'instance creation' }
EpLostChangesDetector class >> newWithLog: aLog [

	^ self basicNew
		initializeWithLog: aLog;
		yourself
]

{ #category : #'system startup' }
EpLostChangesDetector class >> startUp: isImageStarting [

	| detector |
	(isImageStarting and: [ 
		self isEnabled and: [ "Detector can be disabled via system settings"
		EpMonitor hasCurrent and: [ "Nothing to recover if wasn't recording"
		EpMonitor current isEnabled and: [ 
		Smalltalk isHeadless not ]]]])  "Can't browse if UI disabled"
			ifFalse: [ ^ self ].

	detector := self new.
	
	SessionManager default addDeferredStartupAction: [
		(detector hasLostChanges and: [ self confirm: 'It seems your last Pharo session exited without saving some code. Do you want to recover it?' ])
			ifTrue: [ detector openBrowserIfLostChanges ] ]

]

{ #category : #testing }
EpLostChangesDetector >> hasLostChanges [
	
	^ hasLostChanges
]

{ #category : #initialization }
EpLostChangesDetector >> initializeWithLog: aLog [

	self initialize.
	
	lastSessionFileStore := aLog store lowLevelFileStoreIfNone: [ nil ]. "Never nil?"
	hasLostChanges :=
		lastSessionFileStore
			ifNil: [ false ]
			ifNotNil: [ lastSessionFileStore isOutdated ].

]

{ #category : #accessing }
EpLostChangesDetector >> lostChanges [

	| freshLog lastSessionHeadReference entries |
	self hasLostChanges ifFalse: [ ^ #() ]. "Start by fastest available check: file size"
	lastSessionHeadReference := lastSessionFileStore headReference.
	lastSessionFileStore refresh.
	freshLog := EpLog newWithStore: lastSessionFileStore.
	freshLog headReference = lastSessionHeadReference ifTrue: [ ^ #() ]. "Just no lost events"

	"Then, there are lost events"
	entries := freshLog priorEntriesFrom: freshLog headReference upTo: lastSessionHeadReference.

	^ entries ifNotEmpty: [ entries allButLast "reject lastSessionHeadReference's change"]
]

{ #category : #accessing }
EpLostChangesDetector >> openBrowserIfLostChanges [

	| lostChanges lostChangesLog browser |
	lostChanges := self lostChanges collect: #content.
	lostChanges ifEmpty: [ ^ self ].
	lostChangesLog := EpLog newWithStore: (OmMemoryStore withAll: lostChanges).

	browser := EpLogBrowserModel newWithLog: lostChangesLog.
	browser
		refresh;
		title: 'Epicea - Lost Changes Detected';
		openWithSpec.

]
