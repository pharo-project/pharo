Class {
	#name : 'AbstractClassDescriptionAnnouncementTest',
	#superclass : 'AbstractClassDescriptionTest',
	#instVars : [
		'numberOfAnnouncements'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'running' }
AbstractClassDescriptionAnnouncementTest >> setUp [

	super setUp.
	numberOfAnnouncements := 0
]

{ #category : 'running' }
AbstractClassDescriptionAnnouncementTest >> tearDown [

	self class codeChangeAnnouncer unsubscribe: self.
	super tearDown
]

{ #category : 'running' }
AbstractClassDescriptionAnnouncementTest >> when: anAnnouncement do: aBlock [

	self class codeChangeAnnouncer
		when: anAnnouncement
		do: [ :ann |
			numberOfAnnouncements := numberOfAnnouncements + 1.
			aBlock cull: ann ]
		for: self
]
