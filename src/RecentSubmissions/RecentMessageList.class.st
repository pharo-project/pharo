"
MessageList is a simple object that holds a set of ordered method definitions.
See MessageList example for an example.

MessageList example

Instance Variables:
	methodReferenceList	<Collection>
"
Class {
	#name : #RecentMessageList,
	#superclass : #Model,
	#instVars : [
		'methodReferenceList',
		'lastEntry',
		'maxListSize'
	],
	#classVars : [
		'UniqueInstance'
	],
	#category : #'RecentSubmissions-Core'
}

{ #category : #cleanup }
RecentMessageList class >> cleanUp [

	self resetUniqueInstance.
]

{ #category : #icons }
RecentMessageList class >> icon [ 
	^ self iconNamed: #recentMessagesIcon
]

{ #category : #'instance creation' }
RecentMessageList class >> new [
	^ self uniqueInstance
]

{ #category : #'instance creation' }
RecentMessageList class >> open [
	
	self new open.
]

{ #category : #'instance creation' }
RecentMessageList class >> resetUniqueInstance [
	"Reset the singleton of the recentMessageBrowser model, so if you execute it you will lose all the history of the recent messages."
	"self resetUniqueInstance"
	
	self stopNotifications.
	self uniqueInstance triggerEvent: #reset.
	UniqueInstance := nil
	
]

{ #category : #events }
RecentMessageList class >> stopNotifications [
	SystemAnnouncer uniqueInstance unsubscribe: self uniqueInstance
]

{ #category : #'instance creation' }
RecentMessageList class >> uniqueInstance [

	^UniqueInstance ifNil: [UniqueInstance := self basicNew initialize]
]

{ #category : #'event handler' }
RecentMessageList >> aClassHasBeenCommented: anEvent [

	anEvent newStamp ifNil: [ ^self ].
	self addMethodReference: (self createAClassReference: anEvent)
]

{ #category : #'event handler' }
RecentMessageList >> aClassHasBeenRemoved: anEvent [

	self removeAClass: anEvent
]

{ #category : #'event handler' }
RecentMessageList >> aClassHasBeenRenamed: anEvent [

	| toChangeList |
	toChangeList := self methodReferenceList select: [:each | each className = anEvent oldName asSymbol].
	toChangeList do: [:each | each parentName: anEvent newName asSymbol].
	self updateView.
]

{ #category : #'event handler' }
RecentMessageList >> aMethodHasBeenAdded: aMethodEvent [

	self addMethodReference: (self createTheMethodReference: aMethodEvent)

]

{ #category : #'event handler' }
RecentMessageList >> aMethodHasBeenModified: aMethodEvent [

	self addMethodReference: (self createTheMethodReference: aMethodEvent)

]

{ #category : #'event handler' }
RecentMessageList >> aMethodHasBeenRemoved: aMethodEvent [

	self removeAMethod: aMethodEvent

]

{ #category : #actions }
RecentMessageList >> addMethodReference: aMethodReference. [

	self addMethodReferenceSilently: aMethodReference.
	self checkSize.
	self updateView
]

{ #category : #actions }
RecentMessageList >> addMethodReferenceSilently: aMethodReference [

	self methodReferenceList add: aMethodReference.
	lastEntry := aMethodReference.
]

{ #category : #actions }
RecentMessageList >> checkSize [

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
RecentMessageList >> clearAll [

	self methodReferenceList: OrderedCollection new.
	self updateView.
]

{ #category : #private }
RecentMessageList >> createAClassReference: aCommentedEvent [

	^RG2CommentDefinition realClass: aCommentedEvent classCommented
]

{ #category : #private }
RecentMessageList >> createTheMethodReference: aModifiedMethodEvent [
	
	| method |
	method := aModifiedMethodEvent method.

	^ (RG2MethodDefinition
		realClass: method methodClass
		selector: method selector) asHistorical
]

{ #category : #accessing }
RecentMessageList >> defaultMaxListSize [

	^100
]

{ #category : #initialization }
RecentMessageList >> initialize [ 
	"Initialization"
	
	super initialize.
	self clearAll.	
	self registerToAnnouncements.
	
]

{ #category : #accessing }
RecentMessageList >> lastEntry [ 

	^lastEntry
]

{ #category : #accessing }
RecentMessageList >> maxListSize [

	^maxListSize ifNil: [maxListSize := self defaultMaxListSize]
]

{ #category : #accessing }
RecentMessageList >> maxListSize: aNumber [

	maxListSize := aNumber negative ifTrue: [0] ifFalse:[aNumber].
	self checkSize
		ifTrue: [self updateView]
]

{ #category : #accessing }
RecentMessageList >> methodReferenceList [

	^ methodReferenceList
]

{ #category : #accessing }
RecentMessageList >> methodReferenceList: anObject [

	methodReferenceList := anObject
]

{ #category : #userinterface }
RecentMessageList >> open [
	"opens the system window for recent messages"

	^ MessageBrowser new
		topologicSort: false;
		sortingBlock: [ :a :b | a timeStamp > b timeStamp ];
		messages: methodReferenceList;
		title: 'Recently accepted Methods' translated;
		open
]

{ #category : #initialization }
RecentMessageList >> registerToAnnouncements [

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
RecentMessageList >> removeAClass: anEvent [
	
	| tempList |
	tempList := self methodReferenceList select:[:each | each className = anEvent classRemoved name asSymbol].
	tempList do: [:each | self removeMethodReference: each]
]

{ #category : #private }
RecentMessageList >> removeAMethod: aRemovedEvent [
	
	| method list |
	method := aRemovedEvent method.
	list := (self methodReferenceList select: [:each |  
						each className = method methodClass name
							and:[ each name = method selector ]]).
	list do: [:each | each fullName: (each className, '>>', each selector, ' (removed)') ].
	self updateView.
]

{ #category : #actions }
RecentMessageList >> removeMethodReference: aMethodReference [

	self methodReferenceList remove: aMethodReference.
	self updateView.
]

{ #category : #actions }
RecentMessageList >> revertLastMethodSubmission [
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
RecentMessageList >> updateView [
	
	self triggerEvent: #changed.
]
