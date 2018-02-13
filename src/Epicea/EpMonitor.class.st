"
I am a system monitor.

I subscribe to the system announcer to translate announcements to Epicea events (ie. subclasses of EpEvent), and record them into a log.
"
Class {
	#name : #EpMonitor,
	#superclass : #Object,
	#instVars : [
		'systemAnnouncer',
		'log',
		'testAnnouncer',
		'entryReferenceByEvent',
		'jobAnnouncer',
		'announcer',
		'debugMode'
	],
	#classInstVars : [
		'current'
	],
	#category : #'Epicea-Monitor'
}

{ #category : #accessing }
EpMonitor class >> current [

	current ifNil: [
		current := self new ].
	
	^ current 
	
]

{ #category : #accessing }
EpMonitor class >> current: aMonitor [

	current := aMonitor
]

{ #category : #testing }
EpMonitor class >> hasCurrent [
	"Answer if there is an instance of this class that is #current (without creating the instance if one not already exists)."

	^ current isNotNil
]

{ #category : #accessing }
EpMonitor class >> logsDirectory [
	self flag: #todo.	"The base locator could have a wrong fileSystem"

	^ self current sessionStore baseLocator asFileReference
]

{ #category : #'instance creation' }
EpMonitor class >> new [

	^ self newWithLog: EpLog newWithSessionStore
]

{ #category : #'instance creation' }
EpMonitor class >> newWithLog: aLog [

	^ self basicNew 
		initializeWithLog: aLog;
		yourself
]

{ #category : #initialization }
EpMonitor class >> reset [
	"
	self reset
	"

	self hasCurrent ifTrue: [ 
		self current disable.
		self current: nil ]
]

{ #category : #initialization }
EpMonitor class >> restart [
	"
	self restart
	"

	self reset.
	self current enable.
]

{ #category : #private }
EpMonitor >> addEvent: anEvent [ 

	self addEvent: anEvent newEntryDo: [ :newEntry | ]
]

{ #category : #private }
EpMonitor >> addEvent: anEvent newEntryDo: aBlock [

	self 
		addEvent: anEvent
		newEntryDo: aBlock
		triggerReference: self currentTriggerReference
]

{ #category : #private }
EpMonitor >> addEvent: anEvent newEntryDo: aBlock triggerReference: triggerReference [

	| newEntry |
	newEntry := log 
		addEntryWith: anEvent
		tags: [ :tags |
			triggerReference isNull ifFalse: [
				tags 
					at: EpLog triggererReferenceKey
					put: triggerReference ].
			
			tags at: EpLog authorKey put: self author.
			tags at: EpLog timeKey put: self time ].
		
	aBlock value: newEntry.
]

{ #category : #private }
EpMonitor >> announceMonitorStateUpdated [

	self announcer announce: EpMonitorStateUpdated new.
]

{ #category : #accessing }
EpMonitor >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #private }
EpMonitor >> author [
	^ Author uniqueInstance 
		ifUnknownAuthorUse: 'Unknown' 
		during: [ Author uniqueInstance fullName ]

]

{ #category : #accessing }
EpMonitor >> basicLog [
	"Answer a new EpLog with a OmFileStore (not a OmSessionStore as #log)."

	^ EpLog newWithStore: self sessionStore store
]

{ #category : #'announcement handling' }
EpMonitor >> behaviorAdded: aClassAddedAnnoucement [

	self handleAnyErrorDuring: [
		aClassAddedAnnoucement classAdded isTrait
			ifTrue: [ self traitAdded: aClassAddedAnnoucement ]
			ifFalse: [ self classAdded: aClassAddedAnnoucement ] ]
]

{ #category : #'announcement handling' }
EpMonitor >> behaviorModified: aClassModifiedClassDefinitionAnnouncement [

	self handleAnyErrorDuring: [
		aClassModifiedClassDefinitionAnnouncement classAffected isTrait
			ifTrue: [ self traitModified: aClassModifiedClassDefinitionAnnouncement ]
			ifFalse: [ self classModified: aClassModifiedClassDefinitionAnnouncement ] ]
]

{ #category : #'announcement handling' }
EpMonitor >> behaviorRemoved: aClassRemovedAnnoucement [

	self handleAnyErrorDuring: [
		"Workaround: the announcement occurs after class is renamed to AnObsolete 
		and removed from package."
		
		| classRemoved |
		classRemoved := aClassRemovedAnnoucement classRemoved asEpiceaRingDefinition.

		"Fix class name (remove AnObsolete prefix)"
		classRemoved definitionSource: 
			(classRemoved definitionSource
				copyReplaceAll: aClassRemovedAnnoucement classRemoved name printString
				with: aClassRemovedAnnoucement classRemoved originalName printString).

		"Fix class name in the metaclass"
		classRemoved theMetaClass definitionSource: 
			(classRemoved theMetaClass definitionSource
				copyReplaceAll: aClassRemovedAnnoucement classRemoved name
				with: aClassRemovedAnnoucement classRemoved originalName).

		"Fix category"
		classRemoved definitionSource: 
			(classRemoved definitionSource
				copyReplaceAll: 'category: ', 'Unclassified' printString
				with: 'category: ', aClassRemovedAnnoucement categoryName printString).

		classRemoved name: aClassRemovedAnnoucement classRemoved originalName.
		classRemoved category: aClassRemovedAnnoucement categoryName.
		classRemoved package: aClassRemovedAnnoucement packageAffected name.

		aClassRemovedAnnoucement classAffected isTrait
			ifTrue:  [ self traitRemoved: classRemoved ]
			ifFalse: [ self classRemoved: classRemoved ] ]
]

{ #category : #'announcement handling' }
EpMonitor >> categoryAdded: aCategoryAdded [

	self handleAnyErrorDuring: [
		self addEvent: (EpCategoryAddition named: aCategoryAdded categoryName) ]
]

{ #category : #'announcement handling' }
EpMonitor >> categoryRemoved: aPackageCategoryRemoved [

	self handleAnyErrorDuring:[
		self addEvent: (EpCategoryRemoval named: aPackageCategoryRemoved categoryName) ]
]

{ #category : #'announcement handling' }
EpMonitor >> categoryRenamed: aCategoryRenamed [

	self handleAnyErrorDuring: [
		self addEvent: (EpCategoryRename
			oldName: aCategoryRenamed oldCategoryName
			newName: aCategoryRenamed newCategoryName) ]
]

{ #category : #'announcement handling' }
EpMonitor >> classAdded: aClassAddedAnnoucement [

	self addEvent: (EpClassAddition class: aClassAddedAnnoucement classAdded)
]

{ #category : #'announcement handling' }
EpMonitor >> classCommented: anAnnouncement [

	self addEvent: (EpBehaviorCommentChange newWith: anAnnouncement)
]

{ #category : #'announcement handling' }
EpMonitor >> classModified: aClassModifiedAnnouncement [

	self addEvent: 
		(EpClassModification
			oldClass: aClassModifiedAnnouncement oldClassDefinition
			newClass: aClassModifiedAnnouncement newClassDefinition)
]

{ #category : #'announcement handling' }
EpMonitor >> classRecategorized: aClassRecategorizedAnnoucement [

	self handleAnyErrorDuring: [
		aClassRecategorizedAnnoucement newCategory = aClassRecategorizedAnnoucement oldCategory
			ifTrue: [ ^ self ].
			
		self addEvent: (EpBehaviorCategoryChange
			oldCategory: aClassRecategorizedAnnoucement oldCategory
			newCategory: aClassRecategorizedAnnoucement newCategory
			class: aClassRecategorizedAnnoucement classAffected)]
]

{ #category : #'announcement handling' }
EpMonitor >> classRemoved: classRemoved [

	self addEvent: (EpClassRemoval class: classRemoved)
]

{ #category : #'announcement handling' }
EpMonitor >> classRenamed: aClassRenamed [

	self handleAnyErrorDuring: [
		self addEvent: (EpBehaviorNameChange
			oldName: aClassRenamed oldName
			newName: aClassRenamed  newName
			class: aClassRenamed classAffected) ]
]

{ #category : #private }
EpMonitor >> currentTriggerReference [

	Job current 	
		ifNil: [ ^ log nullReference ]
		ifNotNil: [ :currentJob |
			currentJob 
				lookup: [ :job | 
					entryReferenceByEvent at: job 
						ifPresent: [ :reference | ^ reference ] 
						ifAbsent: [ false ] ]
				ifNone: [ ^ log nullReference ].
			].
]

{ #category : #accessing }
EpMonitor >> debugMode [
	^ debugMode ifNil: [ false ]
]

{ #category : #accessing }
EpMonitor >> debugMode: aBoolean [
	debugMode := aBoolean
]

{ #category : #enabling }
EpMonitor >> disable [

	self sessionStore flush.

	{ systemAnnouncer. testAnnouncer. jobAnnouncer }
		do: [ :each | each ifNotNil: [ :a | a unsubscribe: self ] ].

	self announceMonitorStateUpdated.

]

{ #category : #enabling }
EpMonitor >> enable [
	"Enable monitoring IDE announcements."
	
	self isEnabled ifFalse: [ 			
		self subscribeToSystemAnnouncer.
		self subscribeToTestAnnouncer.
		self subscribeToJobAnnouncer. ].

	self announceMonitorStateUpdated.

]

{ #category : #enabling }
EpMonitor >> enabled: aBoolean [

	aBoolean 
		ifTrue: [ self enable ]
		ifFalse: [ self disable ]
]

{ #category : #private }
EpMonitor >> handleAnyErrorDuring: aBlock [

	aBlock on: Error do: [:error | self handleError: error ]
]

{ #category : #private }
EpMonitor >> handleError: error [

	self debugMode 
		ifTrue: [ error signal ]
		ifFalse: [ 'EpMonitor could not log a change. You can enable debugging with "EpMonitor current debugMode: true".' logCr ]
]

{ #category : #'initialize-release' }
EpMonitor >> initializeWithLog: aLog [

	self initialize.

	log := aLog.
	systemAnnouncer := SystemAnnouncer uniqueInstance.
	testAnnouncer := TestCase historyAnnouncer.
	jobAnnouncer := Job jobAnnouncer.
	
	entryReferenceByEvent := IdentityDictionary new.
]

{ #category : #testing }
EpMonitor >> isEnabled [
	^ systemAnnouncer hasSubscriber: self
]

{ #category : #'announcement handling' }
EpMonitor >> jobEnded: aJobEnd [

	| aJob |
	aJob := aJobEnd job.

	"We only care in these cases"
	aJob owner isEpiceaInterestingJobOwner ifFalse: [ ^ self ].

	entryReferenceByEvent
		removeKey: aJob
		ifAbsent: [ "Should not happen, but we do not care :)" ]
]

{ #category : #'announcement handling' }
EpMonitor >> jobStarted: aJobStart [

	| aJob |
	aJob := aJobStart job.

	"We only care in these cases"
	aJob owner isEpiceaInterestingJobOwner ifFalse: [ ^ self ].

	self 
		addEvent: aJob owner asEpiceaEvent 
		newEntryDo: [ :newEntry |
			entryReferenceByEvent
				at: aJob
				put: (log referenceTo: newEntry) ]
]

{ #category : #accessing }
EpMonitor >> log [
	^ log
]

{ #category : #'announcement handling' }
EpMonitor >> logEntryCommented: aCommentModification [

	self addEvent: aCommentModification
]

{ #category : #'announcement handling' }
EpMonitor >> methodAdded: aMethodAddedAnnoucement [

	self handleAnyErrorDuring: [
		self addEvent: (EpMethodAddition method: aMethodAddedAnnoucement methodAffected) ]
]

{ #category : #'announcement handling' }
EpMonitor >> methodModified: aMethodModified [

	self handleAnyErrorDuring: [
		(aMethodModified oldMethod sourceCode = aMethodModified newMethod sourceCode 
			and: [ aMethodModified oldProtocol = aMethodModified newProtocol ])
			ifTrue: [ ^self ].
			
		self addEvent: 
			(EpMethodModification
				oldMethod: aMethodModified oldMethod
				newMethod: aMethodModified newMethod) ]
]

{ #category : #'announcement handling' }
EpMonitor >> methodRecategorized: aMethodRecategorized [

	self handleAnyErrorDuring: [
		| oldMethod newMethod |
		"Workaround: When a trait is removed from the trait composition, 
		then this announcement is wrongly announced."
		
		aMethodRecategorized methodRecategorized ifNil: [ ^ self ].
		oldMethod := aMethodRecategorized methodRecategorized asEpiceaRingDefinition
			protocol: aMethodRecategorized oldProtocol;
			yourself.
		newMethod := aMethodRecategorized methodRecategorized asEpiceaRingDefinition
			protocol: aMethodRecategorized newProtocol;
			yourself.
			
		self addEvent: (EpMethodModification oldMethod: oldMethod newMethod: newMethod)]
]

{ #category : #'announcement handling' }
EpMonitor >> methodRemoved: aMethodRemovedAnnoucement [
	"RPackage already unregistered the method so we have to workaround protocol and package."

	self handleAnyErrorDuring: [
		self addEvent: 
			(EpMethodRemoval method: 
				(aMethodRemovedAnnoucement methodAffected asEpiceaRingDefinition
					protocol: aMethodRemovedAnnoucement protocol;
					package: (RPackageOrganizer default 
						packageForProtocol: aMethodRemovedAnnoucement protocol 
						inClass: aMethodRemovedAnnoucement methodAffected methodClass) name;
					yourself))]
]

{ #category : #'announcement handling' }
EpMonitor >> monticelloVersionSaved: aMCVersionSaved [ 

	self handleAnyErrorDuring: [
		aMCVersionSaved repository description = 'cache' ifFalse: [
			self addEvent: aMCVersionSaved asEpiceaEvent ].
		"Note: Tried to encapsulate this check in the hierarchy of MCRepository 
		but caused troubles to iceberg, since it has a repository that's not in such hierarchy." ]
]

{ #category : #'announcement handling' }
EpMonitor >> protocolAdded: aProtocolAdded [

	self handleAnyErrorDuring: [
		self addEvent: 
			(EpProtocolAddition 
				behavior: aProtocolAdded classAffected
				protocol: aProtocolAdded protocol)]
]

{ #category : #'announcement handling' }
EpMonitor >> protocolRemoved: aProtocolRemoved [ 

	"Skip an irrelevant case"
	aProtocolRemoved protocol = Protocol unclassified ifTrue: [ ^self ].

	self handleAnyErrorDuring: [
		self addEvent: 
			(EpProtocolRemoval
				behavior: aProtocolRemoved classAffected
				protocol: aProtocolRemoved protocol)]
]

{ #category : #'announcement handling' }
EpMonitor >> sessionEnd [
	self addEvent: EpSessionEnd new
]

{ #category : #'announcement handling' }
EpMonitor >> sessionSnapshot [
	self addEvent: EpSessionSnapshot new
]

{ #category : #'announcement handling' }
EpMonitor >> sessionStart [
	self addEvent: EpSessionStart new
]

{ #category : #accessing }
EpMonitor >> sessionStore [

	^ self log store
]

{ #category : #'announcement handling' }
EpMonitor >> snapshotDone: aSnapshotDone [
"-> disable this event as a workaround to case 18374"

"	aSnapshotDone isNewImage 
		ifFalse: [ self sessionSnapshot ]"
]

{ #category : #private }
EpMonitor >> subscribeToJobAnnouncer [

	{	JobStart -> #jobStarted:.
		JobEnd -> #jobEnded:.
	} do: [ :pair |
		jobAnnouncer
			when: pair key 
			send: pair value 
			to: self ].

]

{ #category : #private }
EpMonitor >> subscribeToSystemAnnouncer [

	{	CategoryAdded -> #categoryAdded:. 
		CategoryRemoved -> #categoryRemoved:.
		CategoryRenamed -> #categoryRenamed:.
		ClassAdded-> #behaviorAdded:.
		ClassRemoved->#behaviorRemoved:.
		MethodAdded -> #methodAdded:.
		MethodRemoved -> #methodRemoved:.
		ProtocolAdded -> #protocolAdded:.
		ProtocolRemoved -> #protocolRemoved:.
		ClassModifiedClassDefinition -> #behaviorModified:.
		MethodModified -> #methodModified:.
		ClassRecategorized -> #classRecategorized:.
		ClassRenamed -> #classRenamed:.
		ClassCommented -> #classCommented:.
		MethodRecategorized -> #methodRecategorized:.
		MCVersionSaved	-> #monticelloVersionSaved:.
		
	} do: [ :pair |
		systemAnnouncer weak
			when: pair key 
			send: pair value 
			to: self ].

]

{ #category : #private }
EpMonitor >> subscribeToTestAnnouncer [
	"Hook method. Overriden in EpiceaSUnit."
]

{ #category : #accessing }
EpMonitor >> testAnnouncer [
	^ testAnnouncer
]

{ #category : #private }
EpMonitor >> time [
	^ DateAndTime now
]

{ #category : #'announcement handling' }
EpMonitor >> traitAdded: aClassAddedAnnoucement [

	self addEvent: (EpTraitAddition trait: aClassAddedAnnoucement classAdded)
]

{ #category : #'announcement handling' }
EpMonitor >> traitModified: aClassModifiedClassDefinitionAnnouncement [

	self addEvent: (EpTraitModification
		oldTrait: aClassModifiedClassDefinitionAnnouncement oldClassDefinition
		newTrait: aClassModifiedClassDefinitionAnnouncement newClassDefinition)
]

{ #category : #'announcement handling' }
EpMonitor >> traitRemoved: classRemoved [

	self addEvent: (EpTraitRemoval trait: classRemoved)
]

{ #category : #accessing }
EpMonitor >> writingDeferDuration [

	^ self sessionStore writingDeferDuration
]

{ #category : #accessing }
EpMonitor >> writingDeferDuration: aDuration [

	^ self sessionStore writingDeferDuration: aDuration
]
