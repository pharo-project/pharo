"
I am an abstract class used to gather all the announcements related to class modifications.
"
Class {
	#name : #ClassAnnouncement,
	#superclass : #SystemAnnouncement,
	#category : 'System-Announcements-System-Classes'
}

{ #category : #accessing }
ClassAnnouncement >> classAffected [
	self subclassResponsibility 
]

{ #category : #accessing }
ClassAnnouncement >> classTagAffected [
	^self packageAffected toTagName: self classAffected category
]

{ #category : #accessing }
ClassAnnouncement >> packageAffected [
	^self classAffected package
]

{ #category : #accessing }
ClassAnnouncement >> packagesAffected [
	^{self packageAffected}
]
