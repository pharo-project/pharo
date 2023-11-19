"
This class plays the role of the announcer for events raised by the Pharo system.

BEWARE: You should not need to subclass me. Think about just using me to send your announces (if you need system behavior) or using your own announcers as instance or class  variables.
"
Class {
	#name : 'SystemAnnouncer',
	#superclass : 'Announcer',
	#instVars : [
		'suspended',
		'storedAnnouncements'
	],
	#classInstVars : [
		'announcer'
	],
	#category : 'System-Announcements-Core',
	#package : 'System-Announcements',
	#tag : 'Core'
}

{ #category : 'announcer' }
SystemAnnouncer class >> announce: anAnnouncement [

	^ self uniqueInstance announce: anAnnouncement
]

{ #category : 'accessing' }
SystemAnnouncer class >> announcer: anAnnouncer [
	announcer := anAnnouncer
]

{ #category : 'class initialization' }
SystemAnnouncer class >> reset [
	"unregister all current weak subscriptions because it can cause a memory leak"

	self announcer: nil.
]

{ #category : 'initialization' }
SystemAnnouncer class >> restoreAllNotifications [

	<script>
	self reset.
	(Pragma allNamed: #systemEventRegistration) do: [ :pragma | pragma methodClass instanceSide perform: pragma methodSelector ]
]

{ #category : 'instance creation' }
SystemAnnouncer class >> uniqueInstance [
	"Answer a system annoncer."

	^ announcer ifNil: [ announcer := self new ]
]

{ #category : 'initialization' }
SystemAnnouncer class >> unload [
	self uniqueInstance unsubscribe: self
]

{ #category : 'announce' }
SystemAnnouncer >> announce: anAnnouncement [

	self isSuspended
		ifFalse: [ super announce: anAnnouncement ]
		ifTrue: [ storedAnnouncements ifNotNil: [ storedAnnouncements add: anAnnouncement ] ]
]

{ #category : 'triggering' }
SystemAnnouncer >> class: aClass oldComment: oldComment newComment: newComment oldStamp: oldStamp newStamp: newStamp [
	"A class was commented in the system."

	self announce: (ClassCommented class: aClass oldComment: oldComment newComment: newComment oldStamp: oldStamp newStamp: newStamp)
]

{ #category : 'triggering' }
SystemAnnouncer >> classCommented: aClass [
	"A class with the given name was commented in the system."

	self announce: (ClassCommented classCommented: aClass)
]

{ #category : 'triggering' }
SystemAnnouncer >> classDefinitionChangedFrom: oldClass to: newClass [
	self announce: (ClassModifiedClassDefinition classDefinitionChangedFrom: oldClass to: newClass)
]

{ #category : 'triggering' }
SystemAnnouncer >> classModificationAppliedTo: aClassOrTrait [
	self announce: (ClassModificationApplied toClass: aClassOrTrait)
]

{ #category : 'triggering' }
SystemAnnouncer >> classParentOf: aClass renamedFrom: oldName to: newName [
	self announce: (ClassParentRenamed classParentOf: aClass renamedFrom: oldName to: newName)
]

{ #category : 'triggering' }
SystemAnnouncer >> classRenamed: aClass from: oldClassName to: newClassName [

	self announce: (ClassRenamed class: aClass oldName: oldClassName newName: newClassName)
]

{ #category : 'announce' }
SystemAnnouncer >> delayAnnouncementsAfter: aBlock [
	"I will execute a block and store all the announcements I am making during the execution of this block.
	Once the execution is done, I will announce everything at once."

	(self suspendAllWhileStoring: aBlock) do: [ :announcement | self announce: announcement ]
]

{ #category : 'triggering' }
SystemAnnouncer >> evaluated: textOrStream [
	^ self evaluated: textOrStream context: nil
]

{ #category : 'triggering' }
SystemAnnouncer >> evaluated: expression context: aContext [
	self announce: (ExpressionEvaluated
				expression: expression
				context: aContext)
]

{ #category : 'testing' }
SystemAnnouncer >> isSuspended [
	^suspended ifNil: [ suspended := false ]
]

{ #category : 'triggering' }
SystemAnnouncer >> methodRemoved: aMethod origin: aBehavior [
	"A method with the given selector was removed from the class."

	self announce: (MethodRemoved methodRemoved: aMethod origin: aBehavior)
]

{ #category : 'triggering' }
SystemAnnouncer >> methodRepackaged: aMethod from: aPackage to: anotherPackage [
	self announce: (MethodRepackaged
						methodRepackaged: aMethod
						oldPackage: aPackage
						newPackage: anotherPackage)
]

{ #category : 'triggering' }
SystemAnnouncer >> snapshotDone: isNewImage [

	self announce: (SnapshotDone isNewImage: isNewImage)
]

{ #category : 'announce' }
SystemAnnouncer >> suspendAllWhile: aBlock [
	| oldSuspended |
	oldSuspended := self isSuspended.
	suspended := true.
	^aBlock ensure: [ suspended := oldSuspended ]
]

{ #category : 'announce' }
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

{ #category : 'triggering' }
SystemAnnouncer >> traitDefinitionChangedFrom: oldTrait to: newTrait [
	self announce: (ClassModifiedClassDefinition classDefinitionChangedFrom: oldTrait to: newTrait)
]
