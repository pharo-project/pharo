"
GLMUpdateAction is used for controlling the updating of a presentation when an announcement. is sent by the announcerObjects.

Instance Variables:
	condition	<Boolean>
	presentation	<GLMPresentation>
	announcement	<Announcement>
	announcerObjects	<(Collection of: Objects)>
	transformation	<BlockClosure>
"
Class {
	#name : #GLMUpdateAction,
	#superclass : #Object,
	#instVars : [
		'condition',
		'presentation',
		'announcement',
		'announcerObjects',
		'transformation'
	],
	#category : #'Glamour-Core'
}

{ #category : #'announcement handling' }
GLMUpdateAction >> actOn: anAnnouncement [
	| allowedToTrigger |
	allowedToTrigger := self condition glamourValue: (	
							anAnnouncement asGlamorousMultiValue, 
							self presentation entity asGlamorousMultiValue).
	allowedToTrigger ifTrue: [ self presentation update ]
]

{ #category : #accessing }
GLMUpdateAction >> announcement [
	^ announcement
]

{ #category : #accessing }
GLMUpdateAction >> announcement: anObject [
	announcement := anObject
]

{ #category : #accessing }
GLMUpdateAction >> announcerObjects [ 
	^ announcerObjects := self computeAnnouncerObjects ifNil: [OrderedCollection new]
]

{ #category : #public }
GLMUpdateAction >> computeAnnouncerObjects [ 
	self subclassResponsibility
]

{ #category : #accessing }
GLMUpdateAction >> condition [
	^ condition
]

{ #category : #accessing }
GLMUpdateAction >> condition: anObject [
	condition := anObject
]

{ #category : #initialization }
GLMUpdateAction >> initialize [
	super initialize.
	condition := true
]

{ #category : #copying }
GLMUpdateAction >> postCopy [
	super postCopy.
	announcerObjects := nil
]

{ #category : #accessing }
GLMUpdateAction >> presentation [
	^ presentation
]

{ #category : #accessing }
GLMUpdateAction >> presentation: anObject [
	presentation := anObject
]

{ #category : #public }
GLMUpdateAction >> registerInPresentation [
	self announcerObjects do: [: announcerObject |
		announcerObject notNil ifTrue: [
			(announcerObject when: self announcement send: #actOn: to: self) makeWeak.
			
			"we remember the announcer object to be able to 
			unregister from it when the presentation goes away"
			self presentation registeredAnnouncers add: announcerObject ] ]
]

{ #category : #accessing }
GLMUpdateAction >> transformation [
	^ transformation
]

{ #category : #accessing }
GLMUpdateAction >> transformation: anObject [
	transformation := anObject
]

{ #category : #public }
GLMUpdateAction >> unregisterFromAllAnnouncements [
	self announcerObjects do: [:each | 
		[each unsubscribe: self] on: Error do: [:e | e resume]] 
]
