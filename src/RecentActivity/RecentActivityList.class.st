"
RecentMessageList is a simple object that holds a set of ordered method/class definitions.
It is used by the EmergencyEvaluator (Transcripter) to be able to revert the last method that you 
edited and that totally broke the system. 

We can open the RecentMessageList as a tool (and the dependencies to the message browser is not that nice).
We will have to improve this in the future. 

Instance Variables:
	methodReferenceList	<Collection>
"
Class {
	#name : #RecentActivityList,
	#superclass : #Model,
	#instVars : [
		'methodReferenceList',
		'lastEntry',
		'maxListSize'
	],
	#classVars : [
		'UniqueInstance'
	],
	#category : #RecentActivity
}

{ #category : #cleanup }
RecentActivityList class >> cleanUp [

	self resetUniqueInstance.
]

{ #category : #icons }
RecentActivityList class >> icon [ 
	^ self iconNamed: #recentMessagesIcon
]

{ #category : #initialization }
RecentActivityList class >> initialize [ 
	"Once loaded the recent message is active. 
	Decided not to use the startUp to be able to keep the list over snapshots."
	"self initialize"
	
	self startReceivingNotifications
]

{ #category : #'instance creation' }
RecentActivityList class >> new [
	^ self uniqueInstance
]

{ #category : #'instance creation' }
RecentActivityList class >> open [
	
	self new open
]

{ #category : #menu }
RecentActivityList class >> recentMessageListMenuOn: aBuilder [ 
	<worldMenu> 
	
	(aBuilder item: #'Recent Messages')
		action: [ self  open];
		parent: #Tools;
		order: 720; 
		keyText: 'z';
		help: 'The recent message list is an handy tools to check what you recently did.';
		icon: self icon
	
]

{ #category : #'instance creation' }
RecentActivityList class >> resetUniqueInstance [
	"Reset the singleton of the recentMessageBrowser model, so if you execute it you will lose all the history of the recent messages."
	"self resetUniqueInstance"
	
	self stopReceivingNotifications.
	self uniqueInstance triggerEvent: #reset.
	UniqueInstance := nil
	
]

{ #category : #events }
RecentActivityList class >> startReceivingNotifications [
	"self startReceivingNotifications"
	
	self resetUniqueInstance.
	"to force the iniatialize again since it is the initialize that is doing the registration 
	of the instance to the systemAnnouncer."
	self uniqueInstance
]

{ #category : #events }
RecentActivityList class >> stopReceivingNotifications [
	SystemAnnouncer uniqueInstance unsubscribe: self uniqueInstance
]

{ #category : #'instance creation' }
RecentActivityList class >> uniqueInstance [

	^UniqueInstance ifNil: [UniqueInstance := self basicNew initialize]
]

{ #category : #'event handler' }
RecentActivityList >> aClassHasBeenCommented: anEvent [

	anEvent newStamp ifNil: [ ^self ].
	self addMethodReference: (self createAClassReference: anEvent)
]

{ #category : #'event handler' }
RecentActivityList >> aClassHasBeenRemoved: anEvent [

	self removeAClass: anEvent
]

{ #category : #'event handler' }
RecentActivityList >> aClassHasBeenRenamed: anEvent [

	| toChangeList |
	toChangeList := self methodReferenceList select: [:each | each className = anEvent oldName asSymbol].
	toChangeList do: [:each | each parentName: anEvent newName asSymbol].
	self updateView.
]

{ #category : #'event handler' }
RecentActivityList >> aMethodHasBeenAdded: aMethodEvent [

	self addMethodReference: (self createTheMethodReference: aMethodEvent)

]

{ #category : #'event handler' }
RecentActivityList >> aMethodHasBeenModified: aMethodEvent [

	self addMethodReference: (self createTheMethodReference: aMethodEvent)

]

{ #category : #'event handler' }
RecentActivityList >> aMethodHasBeenRemoved: aMethodEvent [

	self removeAMethod: aMethodEvent

]

{ #category : #actions }
RecentActivityList >> addMethodReference: aMethodReference [.

	self addMethodReferenceSilently: aMethodReference.
	self checkSize.
	self updateView
]

{ #category : #private }
RecentActivityList >> addMethodReferenceSilently: aMethodReference [

	self methodReferenceList add: aMethodReference.
	lastEntry := aMethodReference.
]

{ #category : #private }
RecentActivityList >> checkSize [

	| result |
	result := false.
	self maxListSize > 0 "0 means infinite"
		ifTrue: [
			result := (self methodReferenceList size > self maxListSize).
			[self methodReferenceList size > self maxListSize]
				whileTrue: [self methodReferenceList removeFirst]].
	^result
]

{ #category : #actions }
RecentActivityList >> clearAll [
	"Flush the current kept entities and restart."
	
	self methodReferenceList: OrderedCollection new.
	self updateView.
]

{ #category : #private }
RecentActivityList >> createAClassReference: aCommentedEvent [

	^RGCommentDefinition realClass: aCommentedEvent classCommented
]

{ #category : #private }
RecentActivityList >> createTheMethodReference: aModifiedMethodEvent [
	
	| method methodDef |

	method := aModifiedMethodEvent method.
	methodDef := (RGMethodDefinition realClass: method methodClass selector: method selector).
	method package ifNotNil: [ methodDef package: (RGPackageDefinition new name: method package packageName) ].
	^ methodDef asHistorical
		

]

{ #category : #initialization }
RecentActivityList >> defaultMaxListSize [

	^ 100
]

{ #category : #initialization }
RecentActivityList >> initialize [ 
	"Initialization"
	
	super initialize.
	self clearAll.	
	self registerToAnnouncements.
	
]

{ #category : #accessing }
RecentActivityList >> lastEntry [ 

	^lastEntry
]

{ #category : #accessing }
RecentActivityList >> maxListSize [

	^maxListSize ifNil: [maxListSize := self defaultMaxListSize]
]

{ #category : #accessing }
RecentActivityList >> maxListSize: aNumber [

	maxListSize := aNumber negative ifTrue: [0] ifFalse:[aNumber].
	self checkSize
		ifTrue: [self updateView]
]

{ #category : #accessing }
RecentActivityList >> methodReferenceList [

	^ methodReferenceList
]

{ #category : #private }
RecentActivityList >> methodReferenceList: anObject [

	methodReferenceList := anObject
]

{ #category : #userinterface }
RecentActivityList >> open [
	"opens the system window for recent messages"

	^ MessageBrowser new
		topologicSort: false;
		sortingBlock: [ :a :b | a timeStamp > b timeStamp ];
		messages: methodReferenceList;
		title: 'Recently accepted methods' translated;
		open
]

{ #category : #initialization }
RecentActivityList >> registerToAnnouncements [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak
		"Class"
		when: ClassCommented send: #aClassHasBeenCommented: to: self;
		when: ClassRemoved send: #aClassHasBeenRemoved: to: self;
		when: ClassRenamed send: #aClassHasBeenRenamed: to: self;
		"Method"
		when: MethodAdded send: #aMethodHasBeenAdded: to: self;
		when: MethodModified send: #aMethodHasBeenModified: to: self;
		when: MethodRemoved send: #aMethodHasBeenRemoved: to: self
]

{ #category : #private }
RecentActivityList >> removeAClass: anEvent [
	"When we remove a class, we remove all the methods that belonged to such class."
	| tempList |
	tempList := self methodReferenceList select:[:each | each className = anEvent classRemoved name asSymbol].
	tempList do: [:each | self removeMethodReference: each]
]

{ #category : #private }
RecentActivityList >> removeAMethod: aRemovedEvent [
	
	| method list |
	method := aRemovedEvent method.
	list := (self methodReferenceList select: [:each |  
						each className = method methodClass name
							and:[ each name = method selector ]]).
	list do: [:each | each fullName: (each className, '>>', each selector, ' (removed)') ].
	self updateView.
]

{ #category : #actions }
RecentActivityList >> removeMethodReference: aMethodReference [

	self methodReferenceList remove: aMethodReference.
	self updateView.
]

{ #category : #actions }
RecentActivityList >> revertLastMethodSubmission [
	"If the most recent method submission was a method change, revert
	that change, and if it was a submission of a brand-new method, 
	remove that method."

	"self new revertLastMethodSubmission"

	| changeRecords lastSubmission theClass theSelector |
	self methodReferenceList isEmpty ifTrue: [ ^ self ].
	lastSubmission := self lastEntry.
	theClass := lastSubmission actualClass ifNil: [ ^ self ].
	theSelector := lastSubmission selector.
	changeRecords := SourceFiles changeRecordsFor: lastSubmission.
	changeRecords isEmptyOrNil ifTrue: [ ^ self ].
	changeRecords size = 1
		ifTrue: [ theClass removeSelector: theSelector ] "method has no prior version, so reverting in this case means removing"
		ifFalse: [ changeRecords second fileIn ]
]

{ #category : #userinterface }
RecentActivityList >> updateView [
	
	self triggerEvent: #changed.
]
