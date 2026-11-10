"
I represent a github project present in the metacello project registry that IcebergSystemProjectRecorder has to register in Iceberg.

The github projects are taken directly from the registry: each metacello registration knowing a github repository is listed in Iceberg (with its commit and its baselines) without being cloned. The merging of the metacello registrations of the same repository (one of me per repository), as well as the parsing of the github urls (github://owner/project[:version][/directory]) to find the projects, is done by the recorder.

I hold the fragments of the github repository of the project (owner, project name, source directory and version) and I know how to get the commit sha from the github api (accessed through the #classNamed: method since the loader does not know Iceberg at the time the code is compiled). I know nothing about Iceberg repositories: the registration itself is done by IcebergSystemProjectRecorder.
"
Class {
	#name : 'IcebergSystemProjectToRegister',
	#superclass : 'Object',
	#instVars : [
		'projectName',
		'sourceDirectory',
		'version',
		'registrations',
		'owner'
	],
	#category : 'BaselineOfIDE-Baseline',
	#package : 'BaselineOfIDE',
	#tag : 'Baseline'
}

{ #category : 'instance creation' }
IcebergSystemProjectToRegister class >> owner: anOwner projectName: aProjectName sourceDirectory: aSourceDirectory version: aVersion [
	"Answer a new project to register for the given github repository fragments."

	^ self new
		owner: anOwner;
		projectName: aProjectName;
		sourceDirectory: aSourceDirectory;
		version: aVersion;
		yourself
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> addRegistration: aRegistration [
	registrations add: aRegistration
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> baselines [
	^ registrations collect: [ :registration | registration className asSymbol ]
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> classNamed: aSymbol [
	
	^ self class environment classNamed: aSymbol
]

{ #category : 'resolving' }
IcebergSystemProjectToRegister >> commitSha [
	"Answer the commit sha corresponding to my version in the github repository I represent, first try to get a tag with the version (the regular case), then it has to be a branch so get the branch last commit."

	| tags |
	tags := (self classNamed: #IceGitHubAPI) new getTagsOwner: self owner project: self projectName.

	^ tags
		  detect: [ :tag | (tag at: #name) = self version ]
		  ifFound: [ :tag | (tag at: #commit) at: #sha ]
		  ifNone: [ ((self classNamed: #IceGitHubAPI) new getBranch: self owner project: self projectName name: self version) at: #commit at: #sha ]
]

{ #category : 'initialization' }
IcebergSystemProjectToRegister >> initialize [

	super initialize.
	registrations := OrderedCollection new
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> owner [
	^ owner
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> owner: anOwner [

	owner := anOwner
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> projectName [
	^ projectName
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> projectName: aProjectName [
	projectName := aProjectName
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> repositoryName [
	^ owner , '/' , projectName
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> sourceDirectory [
	^ sourceDirectory
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> sourceDirectory: aSourceDirectory [
	sourceDirectory := aSourceDirectory
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> version [
	"If there is no version MEtacello loads master"

	^ version ifEmpty: [ 'master' ]
]

{ #category : 'accessing' }
IcebergSystemProjectToRegister >> version: aVersion [
	version := aVersion
]
