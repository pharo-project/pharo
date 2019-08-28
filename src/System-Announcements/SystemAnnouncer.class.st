"
This class plays the role of the announcer for events raised by the Pharo system.

BEWARE: You should not need to subclass me. Think about just using me to send your announces (if you need system behavior) or using your own announcers as instance or class  variables.
"
Class {
	#name : #SystemAnnouncer,
	#superclass : #Announcer,
	#instVars : [
		'suspended',
		'private',
		'storedAnnouncements'
	],
	#classInstVars : [
		'announcer'
	],
	#category : #'System-Announcements-Core'
}

{ #category : #accessing }
SystemAnnouncer class >> announcer: anAnnouncer [
	announcer := anAnnouncer
]

{ #category : #initialization }
SystemAnnouncer class >> reset [

	"unregister all current weak subscriptions because it can cause a memory leak"
	announcer subscriptions subscriptions
 		select: [:each | each isKindOf: WeakAnnouncementSubscription] 
		thenDo: [:each | each weakRegistry remove: each subscriber ifAbsent: []]. 
	Smalltalk garbageCollect.
		
	self announcer: nil.
]

{ #category : #initialization }
SystemAnnouncer class >> restoreAllNotifications [
	"self restoreAllNotifications"
	
	| collector |
	self reset.
	collector := PragmaCollector filter: [ :pragma | pragma selector = #systemEventRegistration ].
	collector reset.
	collector do: [ :pragma |
		pragma methodClass instanceSide perform: pragma methodSelector ]
]

{ #category : #'instance creation' }
SystemAnnouncer class >> uniqueInstance [
	"Answer a system annoncer."
	
	^ announcer ifNil: [ announcer := self new ]
]

{ #category : #initialization }
SystemAnnouncer class >> unload [
	self uniqueInstance unsubscribe: self
]

{ #category : #announce }
SystemAnnouncer >> announce: anAnnouncement [
	self isSuspended 
		ifFalse: [ 
			self private announce: anAnnouncement.
			super announce: anAnnouncement ]
		ifTrue:[
			storedAnnouncements ifNotNil:[ storedAnnouncements add: anAnnouncement ]
		].
]

{ #category : #triggering }
SystemAnnouncer >> class: aClass oldComment: oldComment newComment: newComment oldStamp: oldStamp newStamp: newStamp [
	"A class was commented in the system."

	self announce: (ClassCommented class: aClass oldComment: oldComment newComment: newComment oldStamp: oldStamp newStamp: newStamp)
]

{ #category : #triggering }
SystemAnnouncer >> class: aClass recategorizedFrom: oldCategory to: newCategory [ 
	self announce: (ClassRecategorized 
						class: aClass
						recategorizedFrom: oldCategory
						to: newCategory)
]

{ #category : #triggering }
SystemAnnouncer >> classAdded: aClass inCategory: aCategoryName [ 
	self announce: (ClassAdded class: aClass category: aCategoryName)
]

{ #category : #triggering }
SystemAnnouncer >> classCategoryAdded: aClassCategoryName [

	self announce: (CategoryAdded categoryName: aClassCategoryName)
]

{ #category : #triggering }
SystemAnnouncer >> classCategoryRemoved: aClassCategoryName [

	self announce: (CategoryRemoved categoryName: aClassCategoryName)
]

{ #category : #triggering }
SystemAnnouncer >> classCategoryRenamedFrom: anOldClassCategoryName to: aNewClassCategoryName [

	self announce: (CategoryRenamed
					classCategoryRenamedFrom: anOldClassCategoryName 
					to: aNewClassCategoryName)
]

{ #category : #triggering }
SystemAnnouncer >> classCommented: aClass [
	"A class with the given name was commented in the system."

	self announce: (ClassCommented classCommented: aClass)
]

{ #category : #triggering }
SystemAnnouncer >> classDefinitionChangedFrom: oldClass to: newClass [
	self announce: (ClassModifiedClassDefinition classDefinitionChangedFrom: oldClass to: newClass)
]

{ #category : #triggering }
SystemAnnouncer >> classModificationAppliedTo: aClassOrTrait [
	self announce: (ClassModificationApplied toClass: aClassOrTrait)
]

{ #category : #triggering }
SystemAnnouncer >> classParentOf: aClass renamedFrom: oldName to: newName [
	self announce: (ClassParentRenamed classParentOf: aClass renamedFrom: oldName to: newName)
]

{ #category : #triggering }
SystemAnnouncer >> classRemoved: aClass fromCategory: aCategoryName [ 
	self announce: (ClassRemoved class: aClass category: aCategoryName)
]

{ #category : #triggering }
SystemAnnouncer >> classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName [ 
	self announce: (ClassRenamed 
				class: aClass
				category: aCategoryName
				oldName: oldClassName
				newName: newClassName)
]

{ #category : #triggering }
SystemAnnouncer >> classReorganized: aClass [
	self announce: (ClassReorganized class: aClass)
]

{ #category : #triggering }
SystemAnnouncer >> classRepackaged: aClass from: aPackage to: anotherPackage [
	self announce: (ClassRepackaged
						classRepackaged: aClass
						oldPackage: aPackage 
						newPackage: anotherPackage)
]

{ #category : #triggering }
SystemAnnouncer >> classTagAdded: tagName inPackage: aPackage [

	self announce: (ClassTagAdded in: aPackage tag: tagName)
]

{ #category : #triggering }
SystemAnnouncer >> classTagRemoved: tagName fromPackage: aPackage [

	self announce: (ClassTagRemoved in: aPackage tag: tagName)
]

{ #category : #triggering }
SystemAnnouncer >> classTagRenamed: tagName inPackage: aPackage to: newTagName [

	self announce: (ClassTagRenamed in: aPackage tag: tagName to: newTagName)
]

{ #category : #triggering }
SystemAnnouncer >> evaluated: textOrStream [
	^ self evaluated: textOrStream context: nil.
]

{ #category : #triggering }
SystemAnnouncer >> evaluated: expression context: aContext [
	self announce: (ExpressionEvaluated 
				expression: expression
				context: aContext)
]

{ #category : #testing }
SystemAnnouncer >> isSuspended [
	^suspended ifNil: [ suspended := false ]
]

{ #category : #triggering }
SystemAnnouncer >> methodAdded: aMethod [ 
	"A method with the given selector was added to aClass, but not put in a protocol."

	^ self methodAdded: aMethod configuredWith: [ :method | ]
]

{ #category : #triggering }
SystemAnnouncer >> methodAdded: aMethod configuredWith: aBlock [
	"A method with the given selector was added to aClass, but not put in a protocol."

	| event |
	event := MethodAdded method: aMethod.
	aBlock value: event.
	self announce: event.
]

{ #category : #triggering }
SystemAnnouncer >> methodChangedFrom: oldMethod to: newMethod oldProtocol: oldProtocol [

	self announce: (MethodModified methodChangedFrom: oldMethod to: newMethod oldProtocol: oldProtocol)
]

{ #category : #triggering }
SystemAnnouncer >> methodRecategorized: method oldProtocol: oldProtocol [

	self announce: (MethodRecategorized method: method oldProtocol: oldProtocol)
]

{ #category : #triggering }
SystemAnnouncer >> methodRemoved: aMethod protocol: protocol origin: aBehavior [
	"A method with the given selector was removed from the class."

	self announce: (MethodRemoved methodRemoved: aMethod protocol: protocol origin: aBehavior)
]

{ #category : #triggering }
SystemAnnouncer >> methodRepackaged: aMethod from: aPackage to: anotherPackage [
	self announce: (MethodRepackaged
						methodRepackaged: aMethod
						oldPackage: aPackage 
						newPackage: anotherPackage)
]

{ #category : #accessing }
SystemAnnouncer >> private [
	^private ifNil: [ private := Announcer new ]
]

{ #category : #triggering }
SystemAnnouncer >> protocolAdded: aString inClass: aClass [
	self announce: (ProtocolAdded new 
		classReorganized: aClass;
		protocol: aString;
		yourself)
]

{ #category : #triggering }
SystemAnnouncer >> protocolRemoved: aString inClass: aClass [
	self announce: (ProtocolRemoved new 
		classReorganized: aClass;
		protocol: aString;
		yourself)
]

{ #category : #triggering }
SystemAnnouncer >> protocolRenamedFrom: oldName to: newName inClass: aClass [
	self
		announce:
			(ProtocolRenamed new
				classReorganized: aClass;
				oldProtocolName: oldName;
				newProtocolName: newName;
				yourself)
]

{ #category : #triggering }
SystemAnnouncer >> snapshotDone: isNewImage [

	self announce: (SnapshotDone isNewImage: isNewImage)
]

{ #category : #announce }
SystemAnnouncer >> suspendAllWhile: aBlock [
	| oldSuspended |
	oldSuspended := self isSuspended.
	suspended := true.
	^aBlock ensure: [ suspended := oldSuspended ]
]

{ #category : #announce }
SystemAnnouncer >> suspendAllWhileStoring: aBlock [
	| reentring |
	" Suspend all the announcements, storing them in an OrderedCollection, then returns this collection"
	
	reentring := storedAnnouncements isNotNil.
	
	reentring ifFalse:[
		storedAnnouncements := OrderedCollection new.
	].

	[ 
		self suspendAllWhile: aBlock.
		^ storedAnnouncements.
	] ensure:[
		reentring ifFalse:[
			storedAnnouncements := nil.
		]
	]
]

{ #category : #triggering }
SystemAnnouncer >> traitDefinitionChangedFrom: oldTrait to: newTrait [
	self announce: (ClassModifiedClassDefinition classDefinitionChangedFrom: oldTrait to: newTrait)
]

{ #category : #subscription }
SystemAnnouncer >> unsubscribe: anObject [
	self private unsubscribe: anObject.	
	super unsubscribe: anObject.

]
