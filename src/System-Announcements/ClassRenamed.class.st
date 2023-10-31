"
the announcement will be emitted when renaming a class or a trait using:  
	=> RenameClassRefactoring >> rename:to:
	=> class>>rename:
The corresponding event is raised in: SystemDictionary>>renameClass:from:to:
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

{ #category : 'testing' }
ClassRenamed >> affectsMethod: aMethod [
	^self classAffected == aMethod origin
]

{ #category : 'testing' }
ClassRenamed >> affectsMethods [
	"methods belong to class which could be represented by tools as part of method"
	^true
]

{ #category : 'testing' }
ClassRenamed >> affectsMethodsDefinedInClass: aClass [

	^classRenamed == aClass
]

{ #category : 'testing' }
ClassRenamed >> affectsMethodsDefinedInPackage: aPackage [

	^classRenamed package == aPackage
]

{ #category : 'testing' }
ClassRenamed >> affectsMethodsInProtocol: protocol [

	^ classRenamed protocolNames includes: protocol
]

{ #category : 'testing' }
ClassRenamed >> affectsVariablesOf: aClass [
	"Any class has ClassVariables which are visible from both sides: instance side and class 	side.
	That's any class modification can affect variables from both class sides.
	And we do not need to distinguish it"

	aClass instanceSide = classRenamed instanceSide ifTrue: [ ^true ].

	^false
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
