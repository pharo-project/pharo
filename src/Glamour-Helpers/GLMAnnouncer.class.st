"
This is a specialization of the Announcer. The main added functionality is the ability to suspend the announcements from this announcer.
"
Class {
	#name : #GLMAnnouncer,
	#superclass : #Announcer,
	#instVars : [
		'suspendAll',
		'suspendedAnnouncemets'
	],
	#category : #'Glamour-Helpers'
}

{ #category : #announce }
GLMAnnouncer >> announce: anAnnouncement [
	(self isAnnouncementSuspended: anAnnouncement) ifTrue: [ ^ anAnnouncement asAnnouncement ].
	^ super announce: anAnnouncement
]

{ #category : #accessing }
GLMAnnouncer >> announcer [ 
	^ self
]

{ #category : #accessing }
GLMAnnouncer >> glmSubscriptions [
	^ registry subscriptions
]

{ #category : #testing }
GLMAnnouncer >> isAnnouncementSuspended: anAnnouncement [
	^ self suspendAll or: [ |announcementType|
		announcementType := anAnnouncement isClass 
			ifTrue: [ anAnnouncement ] ifFalse: [ anAnnouncement class ].
		self suspendedAnnouncemets anySatisfy: [ :anAnnouncementType |
			"Suspend the announcement if it has the same class or if it inherits from a suspended announcement class."
			announcementType = anAnnouncementType or: [ announcementType  inheritsFrom: anAnnouncementType ]  ] ]
]

{ #category : #copying }
GLMAnnouncer >> postCopy [
	super postCopy.
	registry := registry copy.
]

{ #category : #accessing }
GLMAnnouncer >> resetAnnouncer [
	registry reset
]

{ #category : #suspending }
GLMAnnouncer >> suspendAll [
	^ suspendAll ifNil: [ suspendAll := false ].
]

{ #category : #suspending }
GLMAnnouncer >> suspendAll: aCollection while: aBlock [
	| previousSuspendedAnnouncemets |
	previousSuspendedAnnouncemets := suspendedAnnouncemets.
	suspendedAnnouncemets := aCollection copy.
	aBlock ensure: [suspendedAnnouncemets := previousSuspendedAnnouncemets] 
]

{ #category : #suspending }
GLMAnnouncer >> suspendAllWhile: aBlock [
	| previousSuspensionState |
	previousSuspensionState := suspendAll.
	suspendAll := true.
	aBlock ensure: [suspendAll := previousSuspensionState]
	
]

{ #category : #accessing }
GLMAnnouncer >> suspendedAnnouncemets [
	^ suspendedAnnouncemets ifNil: [ suspendedAnnouncemets := OrderedCollection new ].
]
