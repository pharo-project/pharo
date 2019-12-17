Class {
	#name : #MCAnnouncementTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests-Base'
}

{ #category : #tests }
MCAnnouncementTest >> testMCVersionSaved [
	| announcements version repository |
	version := self mockVersion.
	repository := MCDictionaryRepository new.
	announcements := OrderedCollection new.

	SystemAnnouncer uniqueInstance weak when: MCVersionSaved send: #add: to: announcements.

	repository storeVersion: version.

	SystemAnnouncer uniqueInstance unsubscribe: announcements.

	self assert: announcements size equals: 1.
	self assert: announcements anyOne version identicalTo: version.
	self assert: announcements anyOne repository identicalTo: repository
]
