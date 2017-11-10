"
A dependant project represents an external projetc you are dependending on.

Instance Variables
	- configurationClass : the configurationClass this project represents
	- repository : The core repository URL of this project
"
Class {
	#name : #MTDependantProject,
	#superclass : #MTDependency,
	#instVars : [
		'configurationClass',
		'displayName',
		'repositories',
		'symbolicVersionString'
	],
	#category : #Versionner-Core-DependenciesModel
}

{ #category : #instantiation }
MTDependantProject class >> newWithWorkingCopy: anMBConfigurationInfo [
	"Create a new dependent project with the given project."
	| requiredProject  version |
	
	requiredProject := self new
		name: anMBConfigurationInfo name configurationBaseName;
		configurationClass: anMBConfigurationInfo configurationClass;
		yourself.
	requiredProject displayName: requiredProject name.
	requiredProject editRepositories.
	version := (anMBConfigurationInfo hasVersion: #stable)
		ifTrue: [ #stable ]
		ifFalse: [ #bleedingEdge ].
	requiredProject version: (MTVersion fromVersionString: version).
	^ requiredProject
]

{ #category : #comparing }
MTDependantProject >> = other [
	(super = other) ifFalse: [ ^false ].
	(self configurationClass = other configurationClass) ifFalse: [ ^false ].
	(self displayName = other displayName) ifFalse: [ ^false ].
	(self repositories hasEqualElements: other repositories) ifFalse: [ ^false ].
	^ true
]

{ #category : #visiting }
MTDependantProject >> acceptVisitor: anMTProjectVisitor [
	anMTProjectVisitor visitDependantProject: self
]

{ #category : #accessing }
MTDependantProject >> configurationClass [
	^ configurationClass
]

{ #category : #accessing }
MTDependantProject >> configurationClass: anObject [
	configurationClass := anObject
]

{ #category : #accessing }
MTDependantProject >> displayName [
	^ displayName
]

{ #category : #accessing }
MTDependantProject >> displayName: anObject [
	displayName := anObject
]

{ #category : #displaying }
MTDependantProject >> displayString [
	^ version 
		ifNil: [ displayName ]
		ifNotNil: [ version name 
					ifNil: [ displayName ]
					ifNotNil: [displayName, ' (', version name, ')' ] ]
]

{ #category : #initialize-release }
MTDependantProject >> initialize [ 
	repositories  := OrderedCollection new.
	dependencies := OrderedCollection new.
]

{ #category : #displaying }
MTDependantProject >> repositories [
	^ repositories
]

{ #category : #displaying }
MTDependantProject >> repositories: aRepositoryList [
	"Set a list of repository URLs for this project."
	repositories := aRepositoryList asOrderedCollection.
]

{ #category : #accessing }
MTDependantProject >> symbolicVersionString [
	^ symbolicVersionString
]

{ #category : #accessing }
MTDependantProject >> symbolicVersionString: anObject [
	symbolicVersionString := anObject
]
