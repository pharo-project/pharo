"
This class models the context in which a set of transmissions take place. In essence, it records all ports that were reached after an outside event. 
 
That is necessary for ensuring that transmissions do not get propagated forever. When the first transmission is triggered, a context is created and this context will then store all ports that any subsequent transmission touches. This info is used to break possible cycles

Instance Variables
	ports:		Collection of Ports
"
Class {
	#name : #GLMTransmissionContext,
	#superclass : #Object,
	#instVars : [
		'ports',
		'forbiddenAnnouncements'
	],
	#category : 'Glamour-Core'
}

{ #category : #accessing }
GLMTransmissionContext >> addPort: aPort [
	self ports add: aPort
]

{ #category : #accessing }
GLMTransmissionContext >> can: aPane announce: anAnnouncement [
	^ ((self forbiddenAnnouncements at: aPane ifAbsent: [Set new])
		includes: anAnnouncement ) not
]

{ #category : #accessing }
GLMTransmissionContext >> canAnnounceMatchingPresentationsChanged: aPane [
	^ self can: aPane announce: GLMMatchingPresentationsChanged
]

{ #category : #accessing }
GLMTransmissionContext >> canAnnouncePresentationsChanged: aPane [
	self can: aPane announce: GLMPresentationsChanged
]

{ #category : #accessing }
GLMTransmissionContext >> forbid: aPane toAnnounce: anAnnouncement [
	(self forbiddenAnnouncements at: aPane ifAbsentPut: [Set new])
		add: anAnnouncement
]

{ #category : #accessing }
GLMTransmissionContext >> forbidToAnnounceMatchingPresentationsChanged: aPane [
	self forbid: aPane toAnnounce: GLMMatchingPresentationsChanged
]

{ #category : #accessing }
GLMTransmissionContext >> forbidToAnnouncePresentationsChanged: aPane [
	self forbid: aPane toAnnounce: GLMPresentationsChanged
]

{ #category : #accessing }
GLMTransmissionContext >> forbiddenAnnouncements [
	"forbiddenAnnouncements holds a dictionary in which the key is a pane and the value is a collection of announcements"
	^ forbiddenAnnouncements ifNil: [forbiddenAnnouncements := IdentityDictionary new]
]

{ #category : #accessing }
GLMTransmissionContext >> forbiddenAnnouncements: anObject [
	forbiddenAnnouncements := anObject
]

{ #category : #testing }
GLMTransmissionContext >> includesPort: aPort [
	^ self ports includes: aPort
]

{ #category : #accessing }
GLMTransmissionContext >> ports [
	^ ports ifNil: [ports := OrderedCollection new]
]

{ #category : #accessing }
GLMTransmissionContext >> ports: anObject [
	ports := anObject
]

{ #category : #copying }
GLMTransmissionContext >> postCopy [
	ports := ports copy
]

{ #category : #printing }
GLMTransmissionContext >> printOn: aStream [ 
	super printOn: aStream.
	aStream nextPut: $(.
	ports printOn: aStream.
	aStream nextPut: $)

]
