"
This class plays the role of the announcer for events raised by the Pharo system.

BEWARE: You should not need to subclass me. Think about just using me to send your announces (if you need system behavior) or using your own announcers as instance or class  variables.
"
Class {
	#name : #RGEnvironmentAnnouncer,
	#superclass : #Announcer,
	#instVars : [
		'suspended',
		'private',
		'storedAnnouncements'
	],
	#classInstVars : [
		'announcer'
	],
	#category : #'Ring-Core-Announcements'
}

{ #category : #announce }
RGEnvironmentAnnouncer >> announce: anAnnouncement [
	self isSuspended 
		ifFalse: [ 
			self private announce: anAnnouncement.
			super announce: anAnnouncement ]
		ifTrue:[
			storedAnnouncements ifNotNil:[ storedAnnouncements add: anAnnouncement ]
		].
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorAdded: anRGBehavior [

	self announce: (ClassAdded class: anRGBehavior category: nil)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorCommentModified: anRGBehavior [

	self announce: (ClassCommented classCommented: anRGBehavior)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorDefinitionChangedFrom: oldRGBehavior to: newRGBehavior [

	self announce: (ClassModifiedClassDefinition  
		classDefinitionChangedFrom:  oldRGBehavior
		to: newRGBehavior)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorDefinitionModified: anRGBehavior [

	self announce: (ClassModificationApplied toClass: anRGBehavior)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorModificationAppliedTo: anRGBehavior [

	self announce: (ClassModificationApplied toClass: anRGBehavior)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorParentRenamed: anRGBehavior from: oldName [

	self announce: (ClassParentRenamed 
		classParentOf: anRGBehavior
		renamedFrom: oldName
		to: anRGBehavior name)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorRecategorized: anRGBehavior [ 

	self announce: (ClassRecategorized 
		class: anRGBehavior 
		recategorizedFrom: nil 
		to: anRGBehavior category)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorRemoved: anRGBehavior [

	self announce: (ClassRemoved 
		class: anRGBehavior category: anRGBehavior category)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorRenamed: anRGBehavior from: oldName [

	self announce: (ClassRenamed 
		class: anRGBehavior 
		category: anRGBehavior category 
		oldName: oldName 
		newName: anRGBehavior name)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> behaviorReorganized: anRGBehavior [

	self announce: (ClassReorganized class: anRGBehavior)
]

{ #category : #testing }
RGEnvironmentAnnouncer >> isSuspended [
	^suspended ifNil: [ suspended := false ]
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> methodAdded: aMethod [ 

	self announce: (MethodAdded method: aMethod)
]

{ #category : #triggering }
RGEnvironmentAnnouncer >> methodRemoved: aMethod [ 

	"TODO:"
	self announce: (MethodRemoved methodRemoved: aMethod protocol: nil origin: nil)
]

{ #category : #accessing }
RGEnvironmentAnnouncer >> private [
	^private ifNil: [ private := Announcer new ]
]

{ #category : #announce }
RGEnvironmentAnnouncer >> suspendAllWhile: aBlock [
	| oldSuspended |
	oldSuspended := self isSuspended.
	suspended := true.
	^aBlock ensure: [ suspended := oldSuspended ]
]

{ #category : #announce }
RGEnvironmentAnnouncer >> suspendAllWhileStoring: aBlock [
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

{ #category : #subscription }
RGEnvironmentAnnouncer >> unsubscribe: anObject [
	self private unsubscribe: anObject.	
	super unsubscribe: anObject.

]
