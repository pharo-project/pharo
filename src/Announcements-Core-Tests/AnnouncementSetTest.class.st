"
SUnit tests for announcement sets
"
Class {
	#name : #AnnouncementSetTest,
	#superclass : #TestCase,
	#category : #'Announcements-Core-Tests-Tests'
}

{ #category : #tests }
AnnouncementSetTest >> testIncludeOnlyOnce [
	| set |
	set := AnnouncementMockA , AnnouncementMockB , AnnouncementMockA.
	self assert: set size equals: 2
]

{ #category : #tests }
AnnouncementSetTest >> testInstanceCreation [
	| set |
	set := AnnouncementMockA , AnnouncementMockB.
	self assert: set size equals: 2
]
