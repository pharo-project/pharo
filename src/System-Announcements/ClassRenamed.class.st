"
the announcement will be emitted when renaming a class or a trait using:  
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

{ #category : #testing }
ClassRenamed >> affectsMethod: aMethod [
	^self classAffected == aMethod origin
]

{ #category : #testing }
ClassRenamed >> affectsMethods [
	"methods belong to class which could be represented by tools as part of method"
	^true
]

{ #category : #testing }
ClassRenamed >> affectsMethodsDefinedInClass: aClass [

	^classRenamed == aClass
]

{ #category : #testing }
ClassRenamed >> affectsMethodsDefinedInPackage: aPackage [

	^classRenamed package == aPackage
]

{ #category : #testing }
ClassRenamed >> affectsMethodsTaggedWith: tagName [
	^classRenamed tagsForMethods includes: tagName
]

{ #category : #testing }
ClassRenamed >> affectsVariablesOf: aClass [
	"Any class has ClassVariables which are visible from both sides: instance side and class 	side.
	That's any class modification can affect variables from both class sides.
	And we do not need to distinguish it"

	aClass instanceSide = classRenamed instanceSide ifTrue: [ ^true ].
	
	^false
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

{ #category : #accessing }
ClassRenamed >> packagesAffected [
	^ self classAffected packages
]
