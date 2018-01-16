"
This class plays the role of the announcer for events raised by the Pharo system.

BEWARE: You should not need to subclass me. Think about just using me to send your announces (if you need system behavior) or using your own announcers as instance or class  variables.
"
Class {
	#name : #RG2EnvironmentAnnouncer,
	#superclass : #Announcer,
	#instVars : [
		'suspended',
		'private',
		'storedAnnouncements'
	],
	#classInstVars : [
		'announcer'
	],
	#category : #'Ring2-Core-Announcements'
}

{ #category : #announce }
RG2EnvironmentAnnouncer >> announce: anAnnouncement [
	self isSuspended 
		ifFalse: [ 
			self private announce: anAnnouncement.
			super announce: anAnnouncement ]
		ifTrue:[
			storedAnnouncements ifNotNil:[ storedAnnouncements add: anAnnouncement ]
		].
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorAdded: aRG2BehaviorDefinition [

	self announce: (ClassAdded class: aRG2BehaviorDefinition category: nil)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorCommentModified: aRG2BehaviorDefinition [

	self announce: (ClassCommented classCommented: aRG2BehaviorDefinition)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorDefinitionChangedFrom: oldRG2BehaviorDefinition to: newRG2BehaviorDefinition [

	self announce: (ClassModifiedClassDefinition  
		classDefinitionChangedFrom:  oldRG2BehaviorDefinition
		to: newRG2BehaviorDefinition)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorDefinitionModified: aRG2BehaviorDefinition [

	self announce: (ClassModificationApplied toClass: aRG2BehaviorDefinition)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorModificationAppliedTo: aRG2BehaviorDefinition [

	self announce: (ClassModificationApplied toClass: aRG2BehaviorDefinition)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorParentRenamed: aRG2BehaviorDefinition from: oldName [

	self announce: (ClassParentRenamed 
		classParentOf: aRG2BehaviorDefinition
		renamedFrom: oldName
		to: aRG2BehaviorDefinition name)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorRecategorized: aRG2BehaviorDefinition [ 

	self announce: (ClassRecategorized 
		class: aRG2BehaviorDefinition 
		recategorizedFrom: nil 
		to: aRG2BehaviorDefinition category)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorRemoved: aRG2BehaviorDefinition [

	self announce: (ClassRemoved 
		class: aRG2BehaviorDefinition category: aRG2BehaviorDefinition category)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorRenamed: aRG2BehaviorDefinition from: oldName [

	self announce: (ClassRenamed 
		class: aRG2BehaviorDefinition 
		category: aRG2BehaviorDefinition category 
		oldName: oldName 
		newName: aRG2BehaviorDefinition name)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> behaviorReorganized: aRG2BehaviorDefinition [

	self announce: (ClassReorganized class: aRG2BehaviorDefinition)
]

{ #category : #testing }
RG2EnvironmentAnnouncer >> isSuspended [
	^suspended ifNil: [ suspended := false ]
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> methodAdded: aMethod [ 

	self announce: (MethodAdded method: aMethod)
]

{ #category : #triggering }
RG2EnvironmentAnnouncer >> methodRemoved: aMethod [ 

	"TODO:"
	self announce: (MethodRemoved methodRemoved: aMethod protocol: nil origin: nil)
]

{ #category : #accessing }
RG2EnvironmentAnnouncer >> private [
	^private ifNil: [ private := Announcer new ]
]

{ #category : #announce }
RG2EnvironmentAnnouncer >> suspendAllWhile: aBlock [
	| oldSuspended |
	oldSuspended := self isSuspended.
	suspended := true.
	^aBlock ensure: [ suspended := oldSuspended ]
]

{ #category : #announce }
RG2EnvironmentAnnouncer >> suspendAllWhileStoring: aBlock [
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
RG2EnvironmentAnnouncer >> unsubscribe: anObject [
	self private unsubscribe: anObject.	
	super unsubscribe: anObject.

]
