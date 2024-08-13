"
the announcement will be emitted when renaming a class or a trait using:  
	=> RenameClassRefactoring >> rename:to:
	=> class>>rename:
The corresponding event is raised in: SystemEnvironment>>renameClass:from:to:
"
Class {
	#name : 'ClassRenamed',
	#superclass : 'ClassAnnouncement',
	#instVars : [
		'classRenamed',
		'newName',
		'oldName'
	],
	#category : 'System-Announcements-System-Classes',
	#package : 'System-Announcements',
	#tag : 'System-Classes'
}

{ #category : 'instance creation' }
ClassRenamed class >> class: aClass oldName: anOldClassName newName: aNewClassName [

	^ self new
		  classRenamed: aClass;
		  oldName: anOldClassName;
		  newName: aNewClassName;
		  yourself
]

{ #category : 'accessing' }
ClassRenamed >> classAffected [
	^self classRenamed
]

{ #category : 'accessing' }
ClassRenamed >> classRenamed [

	^ classRenamed
]

{ #category : 'accessing' }
ClassRenamed >> classRenamed: anObject [

	classRenamed := anObject
]

{ #category : 'accessing' }
ClassRenamed >> newName [
	^newName
]

{ #category : 'accessing' }
ClassRenamed >> newName: anObject [

	newName := anObject
]

{ #category : 'accessing' }
ClassRenamed >> oldName [
	^oldName
]

{ #category : 'accessing' }
ClassRenamed >> oldName: anObject [

	oldName := anObject
]

{ #category : 'accessing' }
ClassRenamed >> packagesAffected [
	^ self classAffected packages
]
