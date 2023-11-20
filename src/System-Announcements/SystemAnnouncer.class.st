"
This class plays the role of the announcer for events raised by the Pharo system.

BEWARE: You should not need to subclass me. Think about just using me to send your announces (if you need system behavior) or using your own announcers as instance or class  variables.
"
Class {
	#name : 'SystemAnnouncer',
	#superclass : 'Announcer',
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
