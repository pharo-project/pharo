"
I log the usage data of quality assistant
"
Class {
	#name : #QAEventCollector,
	#superclass : #GTEventCollector,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #QualityAssistantRecording
}

{ #category : #'class initialization' }
QAEventCollector class >> initialize [
	SessionManager default
		register: (ClassSessionHandler forClassNamed: self name)
		inCategory: SessionManager default toolsCategory  
		atPriority: SessionManager default defaultPriority + 1000.
	self recordMeta.

	uniqueInstance := WeakArray new: 1.
	ReSystemAnnouncer uniqueInstance unsubscribe: self.
	ReSystemAnnouncer uniqueInstance weak when: ReCritiqueEvent send: #recordAnnouncement: to: self
]

{ #category : #events }
QAEventCollector class >> recordAnnouncement: anAnnouncement [

	self uniqueInstance recordAnnouncement: anAnnouncement 
]

{ #category : #actions }
QAEventCollector class >> recordMeta [
	
	[ GTEventCollector new
	category: #qualityAssistantMeta;
	addIfAvailable: {
		#nautilus  -> QASettings nautilusPlugin .
		#inspector -> ReSettings inspectorPluggin .
		#spotter   -> ReSettings spotterPlugin
		} asDictionary;
	register
	] on: Error
	  do: [ :error | ReSettings showErrors ifTrue: [ error pass ] ]
]

{ #category : #'system startup' }
QAEventCollector class >> startUp: resuming [
	
	self recordMeta 
]

{ #category : #'instance creation' }
QAEventCollector class >> uniqueInstance [
	^ uniqueInstance first ifNil: [ 
		uniqueInstance at: 1 put: (
			self new
				occupant: ReSystemAnnouncer uniqueInstance;
				register;
				yourself) ]
]

{ #category : #initialize-release }
QAEventCollector class >> unload [

	self uniqueInstance liberate

]

{ #category : #configuration }
QAEventCollector >> defaultCategory [
	^ #qualityAssistantEvents
]

{ #category : #initialization }
QAEventCollector >> initialize [
	super initialize.
	self
		category: #qualityAssistantEvents;
		updateAnnouncementBlock: [ :aGTEventAnnouncement |
			aGTEventAnnouncement at: #qaEventFormatVersion put: 1.1 ]
]

{ #category : #events }
QAEventCollector >> recordAnnouncement: anAnnouncement [
	Privacy sendDiagnosticsAndUsageData ifFalse: [ ^ self ].	
	(anAnnouncement respondsTo: #dataForQAR) ifTrue: [
		[ self add:
			(anAnnouncement dataForQAR
				at: #obfuscated put: Privacy sendEntityNames not;
				at: #timestamp  put: DateAndTime now;
				yourself)
		
		] on: Error
		  do: [ :error |
				ReSettings showErrors ifTrue: [ error pass ] ] ]
]
