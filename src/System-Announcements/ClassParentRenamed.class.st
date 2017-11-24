"
This announcement will be emitted when a class or a traits superclass is renamed.

The class definition and structure may be unaffected if just the name of the superclass changes. But tools
showing the class definition or package organizer depending on the changed class definition string/command and 
may need to be updated or track the changes.
"
Class {
	#name : #ClassParentRenamed,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'classAffected',
		'parentOldName',
		'parentNewName'
	],
	#category : #'System-Announcements-System-Classes'
}

{ #category : #'instance creation' }
ClassParentRenamed class >> classParentOf: aClass renamedFrom: oldName to: newName [
	^ self new
		parentOldName: oldName;
		parentNewName: newName;
		classAffected: aClass;
		yourself
]

{ #category : #accessing }
ClassParentRenamed >> classAffected [
	^ classAffected
]

{ #category : #accessing }
ClassParentRenamed >> classAffected: anObject [
	classAffected := anObject
]

{ #category : #accessing }
ClassParentRenamed >> parentNewName [
	^ parentNewName
]

{ #category : #accessing }
ClassParentRenamed >> parentNewName: anObject [
	parentNewName := anObject
]

{ #category : #accessing }
ClassParentRenamed >> parentOldName [
	^ parentOldName
]

{ #category : #accessing }
ClassParentRenamed >> parentOldName: anObject [
	parentOldName := anObject
]
