"
This class plays the role of the announcer for events raised by the Pharo system.

BEWARE: You should not need to subclass me. Think about just using me to send your announces (if you need system behavior) or using your own announcers as instance or class  variables.
"
Class {
	#name : 'RGEnvironmentAnnouncer',
	#superclass : 'Announcer',
	#category : 'Ring-Core-Announcements',
	#package : 'Ring-Core',
	#tag : 'Announcements'
}

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> behaviorAdded: anRGBehavior [

	self announce: (ClassAdded class: anRGBehavior)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> behaviorCommentModified: anRGBehavior [

	self announce: (ClassCommented classCommented: anRGBehavior)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> behaviorDefinitionChangedFrom: oldRGBehavior to: newRGBehavior [

	self announce: (ClassModifiedClassDefinition
		classDefinitionChangedFrom:  oldRGBehavior
		to: newRGBehavior)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> behaviorDefinitionModified: anRGBehavior [

	self announce: (ClassModificationApplied toClass: anRGBehavior)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> behaviorModificationAppliedTo: anRGBehavior [

	self announce: (ClassModificationApplied toClass: anRGBehavior)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> behaviorParentRenamed: anRGBehavior from: oldName [

	self announce: (ClassParentRenamed
		classParentOf: anRGBehavior
		renamedFrom: oldName
		to: anRGBehavior name)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> behaviorRemoved: anRGBehavior [

	self announce: (ClassRemoved class: anRGBehavior)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> behaviorRenamed: anRGBehavior from: oldName [

	self announce: (ClassRenamed class: anRGBehavior oldName: oldName newName: anRGBehavior name)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> methodAdded: aMethod [

	self announce: (MethodAdded method: aMethod)
]

{ #category : 'triggering' }
RGEnvironmentAnnouncer >> methodRemoved: aMethod [
	"For the protocol we should just send #protocol to the method when #protocol will return a protocol and not a symbol."

	self announce: (MethodRemoved methodRemoved: aMethod origin: aMethod parent)
]
