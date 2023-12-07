"
I am an abstract class used to gather all the announcements related to class modifications.
"
Class {
	#name : 'ClassAnnouncement',
	#superclass : 'SystemAnnouncement',
	#category : 'System-Announcements-System-Classes',
	#package : 'System-Announcements',
	#tag : 'System-Classes'
}

{ #category : 'accessing' }
ClassAnnouncement >> classAffected [
	self subclassResponsibility
]

{ #category : 'accessing' }
ClassAnnouncement >> packageAffected [
	^self classAffected package
]

{ #category : 'accessing' }
ClassAnnouncement >> packageTagAffected [

	^ self classAffected packageTag
]

{ #category : 'accessing' }
ClassAnnouncement >> packageTagsAffected [

	^ { self packageTagAffected }
]

{ #category : 'accessing' }
ClassAnnouncement >> packagesAffected [
	^{self packageAffected}
]
