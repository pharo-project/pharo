"
I represent a GTEventCollector for GTSpotter application.
I am a bit spetial because I send only closed sessions. It means, bundle always end with closing event. The rest of the events are postponed for the next delivery cycle.
"
Class {
	#name : #GTSpotterEventCollector,
	#superclass : #GTEventCollector,
	#instVars : [
		'postponedEvents'
	],
	#category : #'GT-Spotter-EventRecorder-Base'
}

{ #category : #accessing }
GTSpotterEventCollector >> bundle [
	^ self postpone: (self bundle: (postponedEvents ifNil: [ OrderedCollection new ]))
]

{ #category : #configuration }
GTSpotterEventCollector >> defaultCategory [
	^ #spotter
]

{ #category : #initialization }
GTSpotterEventCollector >> linkToSpotter: aSpotterModel [
	aSpotterModel announcer when: Announcement send: #recordAnnouncement: to: self
]

{ #category : #accessing }
GTSpotterEventCollector >> postpone: aCollectionOfEvents [
	| lastExitIndex |
	lastExitIndex := aCollectionOfEvents findLast: [ :each | each isCloseSpotter ].
	lastExitIndex = aCollectionOfEvents size ifTrue: [ 
		postponedEvents := nil.
		^ aCollectionOfEvents ].
	postponedEvents := aCollectionOfEvents copyFrom: lastExitIndex + 1 to: aCollectionOfEvents size.
	^ aCollectionOfEvents first: lastExitIndex.
]

{ #category : #accessing }
GTSpotterEventCollector >> postponedEvents [
	"For testing/debugging purpose."
	^ postponedEvents
]

{ #category : #events }
GTSpotterEventCollector >> recordAnnouncement: anAnnouncement [
	Privacy sendDiagnosticsAndUsageData ifFalse: [ ^ self ].	
	(anAnnouncement respondsTo: #asRecorderEvent) ifTrue: [	
		self add: anAnnouncement asRecorderEvent ]

]

{ #category : #events }
GTSpotterEventCollector >> recordSpotterOpenEvent [
	Privacy sendDiagnosticsAndUsageData ifFalse: [ ^ self ].	
	self add: GTSpotterRecorderOpenEvent new
]
