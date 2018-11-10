"
Adapter for allowing metacello to use Iceberg to download packages and versions.
"
Class {
	#name : #IceMetacelloRepositoryAdapter,
	#superclass : #Object,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-Adapters'
}

{ #category : #'instance creation' }
IceMetacelloRepositoryAdapter class >> for: anIceRepository [ 
	^ self new 
		repository: anIceRepository; 
		yourself
]

{ #category : #comparing }
IceMetacelloRepositoryAdapter >> = other [
	^ other species = self species 
		and: [ other description = self description ]
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> addTo: aMCRepositoryGroup [ 
	"Do nothing, we do not need to register the repository into the monticello registry"
]

{ #category : #testing }
IceMetacelloRepositoryAdapter >> canUpgradeTo: anMCGitBasedRepository [ 
	"Copied from MCGitBasedNetworkRepository"
  (anMCGitBasedRepository isKindOf: self class) ifFalse: [ ^ false ].

  ^ self projectPath = anMCGitBasedRepository projectPath 
	and: [ self repoPath = anMCGitBasedRepository repoPath ]
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> commitIdFor: aWorkingCopy ifFound: aBlock [
	self flag: #todo. "Do nothing?"
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> description [
	^ self repository description
]

{ #category : #initialization }
IceMetacelloRepositoryAdapter >> flushForScriptGet [
	"do nothing"
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> getOrCreateIcebergRepository [
	^ self repository
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> goferPriority [
	^ 8
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> goferReferences [
	^ self repository workingCopy packages 
		collect: [ :package | 
			GoferResolvedReference 
				name: package latestVersion info name 
				repository: self ]
]

{ #category : #testing }
IceMetacelloRepositoryAdapter >> hasNoLoadConflicts: anMCGitBasedRepository [
	"Copied from MCGitBasedNetworkRepository"
  	(anMCGitBasedRepository isKindOf: self class) ifFalse: [ ^ false ].

	^ self projectPath = anMCGitBasedRepository projectPath and: [
		self repoPath = anMCGitBasedRepository repoPath and: [ 
			self projectVersion = anMCGitBasedRepository projectVersion ] ]
]

{ #category : #comparing }
IceMetacelloRepositoryAdapter >> hash [
	"Not a good hash, but is copied from MCRepository"
	^ self description hash
]

{ #category : #compatibility }
IceMetacelloRepositoryAdapter >> isCache [
	"User for Kommiter and Versionner"
	^ false
]

{ #category : #compatibility }
IceMetacelloRepositoryAdapter >> isRemote [
	"User for Komitter"
	^ false
]

{ #category : #testing }
IceMetacelloRepositoryAdapter >> isValid [
	^ true
]

{ #category : #MonticelloGUI }
IceMetacelloRepositoryAdapter >> morphicOpen: workingCopy [
	| shouldOpenIceberg |
	shouldOpenIceberg := UIManager default 
		confirm: ('{1} belongs to {2} and cannot be browsed here. 
Do you want to open the Iceberg Browser instead?' format: { workingCopy packageName. self repository name }) 
		label: 'Browsing an iceberg repository'
		trueChoice: 'Open' 
		falseChoice: 'Cancel'
		cancelChoice: nil
		default: true.
	shouldOpenIceberg ifFalse: [ ^ self ].
	IceTipRepositoriesBrowser new openWithSpec
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> projectPath [
	^ self repository origin projectPath
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> projectVersion [
	^ self repository head gitRef
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> repoPath [
	^ self repository subdirectory 
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> repository [

	^ repository
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> repository: anObject [
	repository := anObject
]

{ #category : #accessing }
IceMetacelloRepositoryAdapter >> repositoryDescription [
	^ self repository description

]

{ #category : #querying }
IceMetacelloRepositoryAdapter >> repositoryVersionString [
	^ self repository workingCopy referenceCommit id
]

{ #category : #compatibility }
IceMetacelloRepositoryAdapter >> unregister [

	^ MCRepositoryGroup default removeRepository: self
]

{ #category : #versions }
IceMetacelloRepositoryAdapter >> versionFrom: aVersionName [ 
	| packageName package version |
	packageName := (MCFileTreeRepository parseName: aVersionName) first.
	package := self repository packageNamed: packageName.
	
	version := package latestVersion.
	version info name = aVersionName ifFalse: [
		version := package versions detect: [ :ver | ver info name = aVersionName ]].
	
	^ version mcVersion
	
	
	
	
]
