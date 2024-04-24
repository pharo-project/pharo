"
I am a system monitor.

I subscribe to the system announcer to translate announcements to Epicea events (ie. subclasses of EpEvent), and record them into a log.
"
Class {
	#name : 'EpMonitor',
	#superclass : 'Object',
	#instVars : [
		'log',
		'entryReferenceByEvent',
		'announcer'
	],
	#classInstVars : [
		'current'
	],
	#category : 'Epicea-Monitor',
	#package : 'Epicea',
	#tag : 'Monitor'
}

{ #category : 'accessing' }
EpMonitor class >> current [

	current ifNil: [
		current := self new ].

	^ current
]

{ #category : 'accessing' }
EpMonitor class >> current: aMonitor [

	current := aMonitor
]

{ #category : 'enabling' }
EpMonitor class >> disableDuring: aBlock [
	"Disable the default monitor during the execution of a block"

	self current disableDuring: aBlock
]

{ #category : 'testing' }
EpMonitor class >> hasCurrent [
	"Answer if there is an instance of this class that is #current (without creating the instance if one not already exists)."

	^ current isNotNil
]

{ #category : 'class initialization' }
EpMonitor class >> initialize [
	SessionManager default registerToolClassNamed: self name
]

{ #category : 'accessing' }
EpMonitor class >> logsDirectory [
	self flag: #pharoFixMe.	"The base locator could have a wrong fileSystem"
	^ self current sessionStore baseLocator asFileReference
]

{ #category : 'instance creation' }
EpMonitor class >> new [

	^ self newWithLog: EpLog newWithSessionStore
]

{ #category : 'instance creation' }
EpMonitor class >> newWithLog: aLog [

	^ self basicNew
		initializeWithLog: aLog;
		yourself
]

{ #category : 'class initialization' }
EpMonitor class >> reset [

	<script>
	self hasCurrent ifFalse: [ ^ self ].

	self current disable.
	self current: nil
]

{ #category : 'initialization' }
EpMonitor class >> restart [

	<script>
	self reset.
	self current enable
]

{ #category : 'system startup' }
EpMonitor class >> shutDown: isImageQuitting [
	(isImageQuitting not
		and: [ self hasCurrent
		and: [ self current isEnabled ] ])
			ifTrue: [
				self current sessionSnapshot.
				self current sessionStore flush. ]
]

{ #category : 'private' }
EpMonitor >> addEvent: anEvent [

	self addEvent: anEvent newEntryDo: [ :newEntry | ]
]

{ #category : 'private' }
EpMonitor >> addEvent: anEvent newEntryDo: aBlock [

	self
		addEvent: anEvent
		newEntryDo: aBlock
		triggerReference: self currentTriggerReference
]

{ #category : 'private' }
EpMonitor >> addEvent: anEvent newEntryDo: aBlock triggerReference: triggerReference [

	| newEntry |
	newEntry := log addEntryWith: anEvent tags: [ :tags |
		            triggerReference isNull ifFalse: [
			            tags
				            at: EpLog triggererReferenceKey
				            put: triggerReference ] ].

	aBlock value: newEntry
]

{ #category : 'private' }
EpMonitor >> announceMonitorStateUpdated [

	self announcer announce: EpMonitorStateUpdated new
]

{ #category : 'accessing' }
EpMonitor >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : 'accessing' }
EpMonitor >> basicLog [
	"Answer a new EpLog with a OmFileStore (not a OmSessionStore as #log)."

	^ EpLog newWithStore: self sessionStore store
]

{ #category : 'announcement handling' }
EpMonitor >> behaviorAdded: aClassAddedAnnouncement [

	aClassAddedAnnouncement classAdded isTrait
		ifTrue: [ self traitAdded: aClassAddedAnnouncement ]
		ifFalse: [ self classAdded: aClassAddedAnnouncement ]
]

{ #category : 'announcement handling' }
EpMonitor >> behaviorModified: aClassModifiedClassDefinitionAnnouncement [

	| event |
	event := aClassModifiedClassDefinitionAnnouncement classAffected isTrait
		         ifTrue: [ self traitModified: aClassModifiedClassDefinitionAnnouncement ]
		         ifFalse: [ self classModified: aClassModifiedClassDefinitionAnnouncement ].

	event hasChanges ifTrue: [ self addEvent: event ]
]

{ #category : 'announcement handling' }
EpMonitor >> behaviorRemoved: aClassRemovedAnnouncement [

	| toReplace classRemoved |
	"Workaround: the announcement occurs after class is renamed to AnObsolete
		and removed from package."
	classRemoved := aClassRemovedAnnouncement classRemoved asEpiceaRingDefinition.

	"Fix class name (remove AnObsolete prefix)"
	classRemoved definitionSource: (classRemoved definitionSource
			 copyReplaceAll: aClassRemovedAnnouncement classRemoved name asString
			 with: aClassRemovedAnnouncement classRemoved originalName asString).

	"Fix class name in the metaclass"
	classRemoved classSide definitionSource:
		(classRemoved classSide definitionSource copyReplaceAll: aClassRemovedAnnouncement classRemoved name with: aClassRemovedAnnouncement classRemoved originalName).

	"Fix category"
	toReplace := (classRemoved definitionSource includesSubstring: '_UnpackagedPackage')
		             ifTrue: [ 'package: ' , '_UnpackagedPackage' printString ]
		             ifFalse: [ 'package: ' , 'Unclassified' printString ].

	classRemoved definitionSource: (classRemoved definitionSource copyReplaceAll: toReplace with: 'package: ' , aClassRemovedAnnouncement packageTagAffected categoryName printString).

	classRemoved name: aClassRemovedAnnouncement classRemoved originalName.
	classRemoved package: aClassRemovedAnnouncement packageAffected name.
	classRemoved packageTag: aClassRemovedAnnouncement packageTagAffected name.

	aClassRemovedAnnouncement classRemoved methods , aClassRemovedAnnouncement classRemoved classSide methods do: [ :each |
		self behaviorRemovedImpliesMethodRemoved: each defaultPackageName: classRemoved package ].

	aClassRemovedAnnouncement classAffected isTrait
		ifTrue: [ self traitRemoved: classRemoved ]
		ifFalse: [ self classRemoved: classRemoved ]
]

{ #category : 'announcement handling' }
EpMonitor >> behaviorRemovedImpliesMethodRemoved: aMethodInAnObsoleteBehavior defaultPackageName: aSymbol [

	| packageName |
	packageName := (self packageOrganizer packageForProtocol: aMethodInAnObsoleteBehavior protocol from: aMethodInAnObsoleteBehavior methodClass) name.

	"If the method is local, (belongs to the class being removed) then the package was wrong,  and we fix it"
	packageName = UndefinedPackage undefinedPackageName ifTrue: [ packageName := aSymbol ].

	self addEvent: (EpMethodRemoval method: (aMethodInAnObsoleteBehavior asEpiceaRingDefinition
				  parentName: aMethodInAnObsoleteBehavior methodClass originalName;
				  protocol: aMethodInAnObsoleteBehavior protocolName;
				  package: packageName;
				  yourself))
]

{ #category : 'announcement handling' }
EpMonitor >> classAdded: aClassAddedAnnouncement [

	self addEvent: (EpClassAddition class: aClassAddedAnnouncement classAdded)
]

{ #category : 'announcement handling' }
EpMonitor >> classCommented: anAnnouncement [

	self addEvent: (EpBehaviorCommentChange newWith: anAnnouncement)
]

{ #category : 'announcement handling' }
EpMonitor >> classModified: aClassModifiedAnnouncement [

	^ EpClassModification
		oldClass: aClassModifiedAnnouncement oldClassDefinition
		newClass: aClassModifiedAnnouncement newClassDefinition
]

{ #category : 'announcement handling' }
EpMonitor >> classRemoved: classRemoved [

	self addEvent: (EpClassRemoval class: classRemoved)
]

{ #category : 'announcement handling' }
EpMonitor >> classRenamed: aClassRenamed [

	self addEvent: (EpBehaviorNameChange oldName: aClassRenamed oldName newName: aClassRenamed newName class: aClassRenamed classAffected)
]

{ #category : 'announcement handling' }
EpMonitor >> classRepackaged: aClassRepackagedAnnouncement [

	self addEvent: (EpBehaviorRepackagedChange
			 oldPackage: aClassRepackagedAnnouncement oldPackage name
			 oldTag: aClassRepackagedAnnouncement oldTag name
			 newPackage: aClassRepackagedAnnouncement newPackage name
			 newTag: aClassRepackagedAnnouncement newTag name
			 class: aClassRepackagedAnnouncement classAffected)
]

{ #category : 'private' }
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
			]
]

{ #category : 'enabling' }
EpMonitor >> disable [

	self sessionStore flush.

	{ self systemAnnouncer. self jobAnnouncer }
		do: [ :each | each ifNotNil: [ :a | a unsubscribe: self ] ].

	self announceMonitorStateUpdated
]

{ #category : 'enabling' }
EpMonitor >> disableDuring: aBlock [
	"Disable all logging during the execution of a block.

	Example:
		EpMonitor current disableDuring: [ self generateApplicationTemplate ]"

	| currentStatus |
	currentStatus := self isEnabled.
	self disable.
	[ aBlock value ]
		ensure: [ self enabled: currentStatus ]
]

{ #category : 'enabling' }
EpMonitor >> enable [
	"Enable monitoring IDE announcements."

	self isEnabled ifFalse: [
		self subscribeToSystemAnnouncer.
		self subscribeToJobAnnouncer. ].

	self announceMonitorStateUpdated
]

{ #category : 'enabling' }
EpMonitor >> enabled: aBoolean [

	aBoolean
		ifTrue: [ self enable ]
		ifFalse: [ self disable ]
]

{ #category : 'initialization' }
EpMonitor >> initializeWithLog: aLog [

	self initialize.

	log := aLog.
	entryReferenceByEvent := IdentityDictionary new
]

{ #category : 'testing' }
EpMonitor >> isEnabled [

	^ self systemAnnouncer hasSubscriber: self
]

{ #category : 'private' }
EpMonitor >> jobAnnouncer [

	^ Job jobAnnouncer
]

{ #category : 'announcement handling' }
EpMonitor >> jobEnded: aJobEnd [

	| aJob |
	aJob := aJobEnd job.

	"We only care in these cases"
	aJob owner isEpiceaInterestingJobOwner ifFalse: [ ^ self ].

	entryReferenceByEvent
		removeKey: aJob
		ifAbsent: [ "Should not happen, but we do not care :)" ]
]

{ #category : 'announcement handling' }
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

{ #category : 'accessing' }
EpMonitor >> log [
	^ log
]

{ #category : 'announcement handling' }
EpMonitor >> logEntryCommented: aCommentModification [

	self addEvent: aCommentModification
]

{ #category : 'announcement handling' }
EpMonitor >> methodAdded: aMethodAddedAnnouncement [

	self addEvent: (EpMethodAddition method: aMethodAddedAnnouncement methodAffected)
]

{ #category : 'announcement handling' }
EpMonitor >> methodModified: aMethodModified [

	(aMethodModified oldMethod sourceCode = aMethodModified newMethod sourceCode and: [ aMethodModified oldProtocol = aMethodModified newProtocol ]) ifTrue: [
		^ self ].

	self addEvent: (EpMethodModification oldMethod: aMethodModified oldMethod newMethod: aMethodModified newMethod)
]

{ #category : 'announcement handling' }
EpMonitor >> methodRecategorized: aMethodRecategorized [

	| oldMethod newMethod |
	"Workaround: When a trait is removed from the trait composition,
		then this announcement is wrongly announced."
	aMethodRecategorized methodRecategorized ifNil: [ ^ self ].
	oldMethod := aMethodRecategorized methodRecategorized asEpiceaRingDefinition
		             protocol: aMethodRecategorized oldProtocol name;
		             yourself.
	newMethod := aMethodRecategorized methodRecategorized asEpiceaRingDefinition
		             protocol: aMethodRecategorized newProtocol name;
		             yourself.

	self addEvent: (EpMethodModification oldMethod: oldMethod newMethod: newMethod)
]

{ #category : 'announcement handling' }
EpMonitor >> methodRemoved: aMethodRemovedAnnouncement [
	"Package already unregistered the method so we have to workaround protocol."

	self addEvent: (EpMethodRemoval method: aMethodRemovedAnnouncement methodAffected asEpiceaRingDefinition)
]

{ #category : 'announcement handling' }
EpMonitor >> packageAdded: packageAdded [

	self addEvent: (EpPackageAddition packageName: packageAdded package name)
]

{ #category : 'announcement handling' }
EpMonitor >> packageRemoved: packageRemoved [

	self addEvent: (EpPackageRemoval packageName: packageRemoved package name)
]

{ #category : 'announcement handling' }
EpMonitor >> packageRenamed: aPackageRenamed [

	self addEvent: (EpPackageRename oldName: aPackageRenamed oldName newName: aPackageRenamed newName)
]

{ #category : 'announcement handling' }
EpMonitor >> packageTagAdded: aPackageTagAddition [

	self addEvent: (EpPackageTagAddition tagName: aPackageTagAddition tag name packageName: aPackageTagAddition package name)
]

{ #category : 'announcement handling' }
EpMonitor >> packageTagRemoved: aPackageTagRemoved [

	self addEvent: (EpPackageTagRemoval tagName: aPackageTagRemoved tag name packageName: aPackageTagRemoved package name)
]

{ #category : 'announcement handling' }
EpMonitor >> packageTagRenamed: aPackageTagRenamed [

	self addEvent: (EpPackageTagRename oldName: aPackageTagRenamed oldName newName: aPackageTagRenamed newName packageName: aPackageTagRenamed package name)
]

{ #category : 'announcement handling' }
EpMonitor >> protocolAdded: aProtocolAdded [

	self addEvent: (EpProtocolAddition behavior: aProtocolAdded classAffected protocol: aProtocolAdded protocol name)
]

{ #category : 'announcement handling' }
EpMonitor >> protocolRemoved: aProtocolRemoved [
	"Skip an irrelevant case"

	aProtocolRemoved protocol isUnclassifiedProtocol ifTrue: [ ^ self ].


	self addEvent: (EpProtocolRemoval behavior: aProtocolRemoved classAffected protocol: aProtocolRemoved protocol name)
]

{ #category : 'announcement handling' }
EpMonitor >> sessionEnd [
	self addEvent: EpSessionEnd new
]

{ #category : 'announcement handling' }
EpMonitor >> sessionSnapshot [
	self addEvent: EpSessionSnapshot new
]

{ #category : 'announcement handling' }
EpMonitor >> sessionStart [
	self addEvent: EpSessionStart new
]

{ #category : 'accessing' }
EpMonitor >> sessionStore [

	^ self log store
]

{ #category : 'private' }
EpMonitor >> subscribeToJobAnnouncer [

	{	JobStart -> #jobStarted:.
		JobEnd -> #jobEnded:.
	} do: [ :pair |
		self jobAnnouncer
			when: pair key
			send: pair value
			to: self ]
]

{ #category : 'private' }
EpMonitor >> subscribeToSystemAnnouncer [

	{
		(PackageAdded -> [ :ann | self packageAdded: ann ]).
		(PackageRenamed -> [ :ann | self packageRenamed: ann ]).
		(PackageRemoved -> [ :ann | self packageRemoved: ann ]).
		(PackageTagAdded -> [ :ann | self packageTagAdded: ann ]).
		(PackageTagRemoved -> [ :ann | self packageTagRemoved: ann ]).
		(PackageTagRenamed -> [ :ann | self packageTagRenamed: ann ]).
		(ClassAdded -> [ :ann | self behaviorAdded: ann ]).
		(ClassRemoved -> [ :ann | self behaviorRemoved: ann ]).
		(MethodAdded -> [ :ann | self methodAdded: ann ]).
		(MethodRemoved -> [ :ann | self methodRemoved: ann ]).
		(ProtocolAdded -> [ :ann | self protocolAdded: ann ]).
		(ProtocolRemoved -> [ :ann | self protocolRemoved: ann ]).
		(ClassModifiedClassDefinition -> [ :ann | self behaviorModified: ann ]).
		(MethodModified -> [ :ann | self methodModified: ann ]).
		(ClassRepackaged -> [ :ann | self classRepackaged: ann ]).
		(ClassRenamed -> [ :ann | self classRenamed: ann ]).
		(ClassCommented -> [ :ann | self classCommented: ann ]).
		(MethodRecategorized -> [ :ann | self methodRecategorized: ann ]) } asDictionary keysAndValuesDo: [ :announcement :block |
		self systemAnnouncer weak
			when: announcement
			do: [ :ann | "During the tests, we should only log with Epicea if the test case declare it wants logging."
				(CurrentExecutionEnvironment value isTest and: [ CurrentExecutionEnvironment value testCase shouldLogWithEpicea not ]) ifFalse: [ block value: ann ] ]
			for: self ]
]

{ #category : 'private' }
EpMonitor >> systemAnnouncer [

	^ self class codeChangeAnnouncer
]

{ #category : 'private' }
EpMonitor >> time [
	^ DateAndTime now
]

{ #category : 'announcement handling' }
EpMonitor >> traitAdded: aClassAddedAnnouncement [

	self addEvent: (EpTraitAddition trait: aClassAddedAnnouncement classAdded)
]

{ #category : 'announcement handling' }
EpMonitor >> traitModified: aClassModifiedClassDefinitionAnnouncement [

	^ EpTraitModification
		oldTrait: aClassModifiedClassDefinitionAnnouncement oldClassDefinition
		newTrait: aClassModifiedClassDefinitionAnnouncement newClassDefinition
]

{ #category : 'announcement handling' }
EpMonitor >> traitRemoved: classRemoved [

	self addEvent: (EpTraitRemoval trait: classRemoved)
]

{ #category : 'accessing' }
EpMonitor >> writingDeferDuration [

	^ self sessionStore writingDeferDuration
]

{ #category : 'accessing' }
EpMonitor >> writingDeferDuration: aDuration [

	^ self sessionStore writingDeferDuration: aDuration
]
