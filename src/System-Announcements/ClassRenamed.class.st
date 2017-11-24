"
the annoucement will be emitted when renaming a class or a trait using:  
	=> RenameClassRefactoring >> rename:to:
	=> class>>rename:
The corresponding event is raised in: SystemDictionary>>renameClass:from:to:
"
Class {
	#name : #ClassRenamed,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'category',
		'classRenamed',
		'newName',
		'oldName'
	],
	#category : #'System-Announcements-System-Classes'
}

{ #category : #'instance creation' }
ClassRenamed class >> class: aClass category: aCategoryName oldName: anOldClassName newName: aNewClassName [
	^self new
			classRenamed: aClass;
			category: aCategoryName;
			oldName: anOldClassName;
			newName: aNewClassName;
			yourself
]

{ #category : #accessing }
ClassRenamed >> category [
	
	^ category
]

{ #category : #accessing }
ClassRenamed >> category: anObject [
	
	category := anObject
]

{ #category : #accessing }
ClassRenamed >> classAffected [
	^self classRenamed
]

{ #category : #accessing }
ClassRenamed >> classRenamed [
	
	^ classRenamed
]

{ #category : #accessing }
ClassRenamed >> classRenamed: anObject [
	
	classRenamed := anObject
]

{ #category : #accessing }
ClassRenamed >> newName [
	^newName
]

{ #category : #accessing }
ClassRenamed >> newName: anObject [
	
	newName := anObject
]

{ #category : #accessing }
ClassRenamed >> oldName [
	^oldName
]

{ #category : #accessing }
ClassRenamed >> oldName: anObject [
	
	oldName := anObject
]
