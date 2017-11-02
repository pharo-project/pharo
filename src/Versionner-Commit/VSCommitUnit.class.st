"
I'm a commit unit for a configuration handled with Versionner. 

A commit unit englobates all actions needed to perform a successful commit from a new version. 
Essentially, it will collect all dirty packages and its corresponfing configurations and it will: 

1) commit dirty packages
2) generate new configuration versions
3) commit new configurations

Example:
========
A commit unit can create versions  by working over configurations, so the best way to act is doing something like this: 

""This will create a new major version""
(VSCommitUnit major: ConfigurationOfVersionner) execute.
""This will create a new minor version""
(VSCommitUnit patch: ConfigurationOfVersionner) execute.
""This will create a new patch version""
(VSCommitUnit patch: ConfigurationOfVersionner) execute.
"
Class {
	#name : #VSCommitUnit,
	#superclass : #Object,
	#instVars : [
		'configurationClass',
		'comment',
		'repository',
		'commitVersion',
		'commitStrategy',
		'projectActions',
		'packageActions'
	],
	#category : 'Versionner-Commit'
}

{ #category : #'instance creation' }
VSCommitUnit class >> configurationClass: aConfigurationClass version: aVersion [
	^ self basicNew 
		initializeConfigurationClass: aConfigurationClass version: aVersion;
		yourself
]

{ #category : #'instance creation' }
VSCommitUnit class >> major: aClass [
	^ self 
		configurationClass: aClass 
		version: VSCommitVersion major
]

{ #category : #'instance creation' }
VSCommitUnit class >> minor: aClass [
	^ self 
		configurationClass: aClass 
		version: VSCommitVersion minor
]

{ #category : #'instance creation' }
VSCommitUnit class >> new [
	self error: 'Use #configurationClass:'
]

{ #category : #'instance creation' }
VSCommitUnit class >> patch: aClass [
	^ self 
		configurationClass: aClass 
		version: VSCommitVersion patch
]

{ #category : #accessing }
VSCommitUnit >> allActions [
	^ self projectActions reversed, self packageActions
]

{ #category : #private }
VSCommitUnit >> collectCommitActions [
	| developmentVersion project collector |
	
	(VSCommitProjectValidator basename: (self configurationClass name configurationBaseName)) validate.
	developmentVersion := self configurationClass project version: #development. 
	project := MTProject
		newFromVersion: developmentVersion
		inConfiguration: self configurationClass.

	collector := VSDirtyCollector strategy: self commitStrategy.
	project acceptVisitor: collector.

	projectActions := collector projects collect: #asCommitAction as: Array.
	"Force main project at least"
	(projectActions isEmpty and: [ self commitStrategy shouldCommitProjects ]) 
		ifTrue: [ projectActions := { project asCommitAction } ].
	packageActions := collector packages collect: #asCommitAction as: Array
]

{ #category : #accessing }
VSCommitUnit >> comment [
	^ comment
]

{ #category : #accessing }
VSCommitUnit >> comment: anObject [
	comment := anObject
]

{ #category : #accessing }
VSCommitUnit >> commitStrategy [
	^ commitStrategy
]

{ #category : #accessing }
VSCommitUnit >> commitStrategy: aStrategy [
	commitStrategy := aStrategy.
	self refresh
]

{ #category : #accessing }
VSCommitUnit >> commitVersion [
	^ commitVersion
]

{ #category : #accessing }
VSCommitUnit >> configurationClass [
	^ configurationClass
]

{ #category : #executing }
VSCommitUnit >> execute [
	self validate.
	"First commit packages"
	self packageActions do: [ :each | each executeOn: self ].
	"Now we can generate new versions and commit configs"
	self projectActions do: [ :each | each executeOn: self ]
]

{ #category : #initialization }
VSCommitUnit >> initialize [ 
	super initialize.
	commitStrategy := VSCommitStrategy new.
	self collectCommitActions
]

{ #category : #initialization }
VSCommitUnit >> initializeConfigurationClass: aClass  version: aVersion [
	configurationClass := aClass.
	commitVersion := aVersion.
	self initialize
]

{ #category : #showing }
VSCommitUnit >> open [
	^ VSCommitBrowser openOn: self
]

{ #category : #accessing }
VSCommitUnit >> packageActions [
	^ packageActions
]

{ #category : #accessing }
VSCommitUnit >> project [
	^ (self projectActions 
		detect: [ :each | each project configurationClass = self configurationClass ]
		ifNone: [ ^ nil ])
		project
		
]

{ #category : #accessing }
VSCommitUnit >> projectActions [
	^ projectActions
]

{ #category : #accessing }
VSCommitUnit >> projectVersion [
	| project workingCopy info |
	project := self project ifNil: [ ^ nil ].
	workingCopy := project workingCopy.
	info := workingCopy ancestors first.
	^ MCVersion  package: workingCopy info: info.

]

{ #category : #accessing }
VSCommitUnit >> refresh [
	self collectCommitActions
]

{ #category : #accessing }
VSCommitUnit >> removePackageAction: anAction [ 
	packageActions := packageActions copyWithout: anAction
]

{ #category : #accessing }
VSCommitUnit >> removeProjectAction: anAction [ 
	projectActions := projectActions copyWithout: anAction
]

{ #category : #accessing }
VSCommitUnit >> repository [
	^ repository
]

{ #category : #accessing }
VSCommitUnit >> repository: anObject [
	repository := anObject
]

{ #category : #validation }
VSCommitUnit >> validate [
	self projectActions do: #validate.
	self packageActions do: #validate.

]
