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

{ #category : 'testing' }
ClassAnnouncement >> affectsClass: aClass [
	^self classAffected == aClass
]

{ #category : 'testing' }
ClassAnnouncement >> affectsClasses [
	^true
]

{ #category : 'testing' }
ClassAnnouncement >> affectsClassesDefinedInPackage: aPackage [
	^self packagesAffected includes: aPackage
]

{ #category : 'testing' }
ClassAnnouncement >> affectsClassesExtendedInPackage: aPackage [
	^aPackage extendsClass: self classAffected
]

{ #category : 'testing' }
ClassAnnouncement >> affectsPackages [
	^true
]

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
