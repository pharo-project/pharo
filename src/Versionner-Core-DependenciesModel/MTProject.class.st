"
A project represents a software development project, and more presicely, its depdendencies (also known as configuration).

Note: Need to add extra methods with ""for:"" parameter to handle platforms (e.g. for: #'pharo' do)

Instance Variables
	- configurationClass : the configurationClass this project represents
	- repository : The core repository URL of this project
	- packages : internal packages (MTPackage) the project depends on
	- groups : definitions of set of dependencies (MTGroups)
	- depedentProjects : list of external projects this project depends on (list of project names)

"
Class {
	#name : #MTProject,
	#superclass : #Object,
	#instVars : [
		'name',
		'version',
		'configurationClass',
		'repository',
		'dependencies',
		'devWorkflow',
		'announcer'
	],
	#category : 'Versionner-Core-DependenciesModel'
}

{ #category : #'instance creation' }
MTProject class >> newFromVersion: aMetacelloVersion inConfiguration: aConfigurationClass [
	^ MCModel2MTModelVisitor new
		visitConfiguration: aConfigurationClass
		withVersion: aMetacelloVersion
]

{ #category : #'instance creation' }
MTProject class >> newNamed: projectName withInitialVersion: version inRepository: repositoryURL [
	"Create the skeleton of a new project (alos known as configuration)"
	| project |

	project := self new.
	project name: projectName.
	project repository: repositoryURL.
	project configurationClass: project devWorkflow createInitialDevelopment.
	^ project.
	
]

{ #category : #comparing }
MTProject >> = other [
	(self name = other name) ifFalse: [ ^false ].
	(self version = other version) ifFalse: [ ^false ].
	(self configurationClass  = other configurationClass) ifFalse: [ ^false ].
	(self repository = other repository) ifFalse: [ ^false ].
	^true
]

{ #category : #visiting }
MTProject >> acceptVisitor: anMTProjectVisitor [
	anMTProjectVisitor visitProject: self.
	dependencies do: [ :dependency | dependency acceptVisitor: anMTProjectVisitor ]
]

{ #category : #setting }
MTProject >> addDependency: aDependency [
	"Add a dependency to the configuration (will create a new baseline)."

	dependencies add: aDependency.
	aDependency parent: self.
	self announcer announce: (MTProjectModelChanged project: self).
]

{ #category : #accessing }
MTProject >> announcer [
	^ announcer ifNil: [announcer := Announcer new]
]

{ #category : #accessing }
MTProject >> configurationClass [
	^ configurationClass
]

{ #category : #protocol }
MTProject >> configurationClass: aConfigurationClass [
	configurationClass := aConfigurationClass.
	self name: configurationClass name configurationBaseName.

]

{ #category : #accessing }
MTProject >> currentBaseline [
	| baselines |
	
	baselines := self configurationClass project versions select: [:aVersion | aVersion blessing == #baseline].
	^baselines last
]

{ #category : #accessing }
MTProject >> dependencies [
	^dependencies .
]

{ #category : #accessing }
MTProject >> dependencies: aDependencyList [
	dependencies := aDependencyList
]

{ #category : #private }
MTProject >> dependenciesFilteredBy: aClass [
	"Filter dependencies to only get Class instances"
	^ (dependencies select: [ :each | each class =  aClass ]) sorted
]

{ #category : #accessing }
MTProject >> dependenciesMap [
	^ self packages collect: [ :aPackage | aPackage name -> aPackage dependencies ]
]

{ #category : #accessing }
MTProject >> dependencyNamed: aDependencyName [
	"Get a depedency from its name"
	
	^ dependencies
		detect: [  :aDependency | (aDependency name =  aDependencyName) or: [ aDependency displayName =  aDependencyName ] ]
		ifNone: [ nil ]

]

{ #category : #accessing }
MTProject >> devWorkflow [
	^ devWorkflow
		ifNil: [ devWorkflow := MTDevelopmentWorkfow newWithProject: self ]
		ifNotNil: [ :myself | myself ]
]

{ #category : #accessing }
MTProject >> groups [
	"Filter dependencies to only get groups"
	^ self dependenciesFilteredBy: MTGroup .
]

{ #category : #initialize-release }
MTProject >> initialize [ 
	dependencies  := OrderedCollection new.

]

{ #category : #accessing }
MTProject >> name [
	^ name
]

{ #category : #accessing }
MTProject >> name: anObject [
	name := anObject
]

{ #category : #accessing }
MTProject >> packages [
	"Filter dependencies to only get packages"
	^ self dependenciesFilteredBy: MTPackage .
]

{ #category : #protocol }
MTProject >> removeDependencyNamed: aDependencyName [
	"Remove a depedency from the configuration"
	
	(self dependencyNamed: aDependencyName) remove
]

{ #category : #accessing }
MTProject >> repository [ 
	^ repository 
]

{ #category : #accessing }
MTProject >> repository: aRepositoryURL [
	repository := aRepositoryURL.
]

{ #category : #accessing }
MTProject >> requiredProjects [
	"Filter dependencies to only get RquiredProject"
	^ self dependenciesFilteredBy: MTDependantProject .
]

{ #category : #accessing }
MTProject >> version [
	^ version
]

{ #category : #setting }
MTProject >> version: aMTVersion [ 
	version := aMTVersion
]
