"
I create repositories. 
I do this (trying in this order):

- with a location and url defined, I check if it can be a repository and in positive case, I answer the repository, or I can do a clone+checkout (into location) and answer the resulting repository.
- with a remote defined , I can take url and location and perform the correct actions.

You can also define  branchName to checkout a specific branch.
"
Class {
	#name : #IceRepositoryCreator,
	#superclass : #Object,
	#instVars : [
		'location',
		'url',
		'remote',
		'branchName',
		'subdirectory',
		'repository',
		'ensureProjectFile'
	],
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #accessing }
IceRepositoryCreator class >> defaultSubdirectory [
	^ ''
]

{ #category : #testing }
IceRepositoryCreator class >> isGitRoot: aReference [
	self flag: #todo. "This does not understand submodules"
	^ aReference isDirectory 
		and: [ (aReference directoriesMatching: '.git') notEmpty
		and: [ (aReference / '.git' / 'config') exists ] ]
]

{ #category : #'private actions' }
IceRepositoryCreator >> addLocalRepository [
	self repository location: self locationToUse.
	self repository pluginManager repositoryWillBeCreated: self repository.
			
	self repository workingCopy initializePackagesFromRepository.
	self doEnsureProject. 	

	self repository pluginManager repositoryWasCreated: self repository.
	^ self repository	
]

{ #category : #'private actions' }
IceRepositoryCreator >> addLocalRepositoryAndCheckout [
	| localRepository |
	localRepository := self addLocalRepository.
	localRepository remotes 
		detect:[ :each | each referencesSameRemoteLocationAs: self remoteToUse ]
		ifFound: [ :found | localRepository fetchFrom: found ].
	localRepository switchToCommitishNamed: self commitishName.
	^ localRepository
]

{ #category : #private }
IceRepositoryCreator >> addProjectFileTo: aRepository [
	
	| newProject |
	newProject := IceBasicProject onRepository: aRepository.
	newProject sourceDirectory: self subdirectory.
	aRepository workingCopy basicSetProject: newProject.
]

{ #category : #private }
IceRepositoryCreator >> addPropertiesFileTo: aRepository [
	aRepository properties fileFormat: self formatToUse
]

{ #category : #remotes }
IceRepositoryCreator >> addRemote: aString [ 
	
	self remote: (IceGitRemote url: aString)
]

{ #category : #'private actions' }
IceRepositoryCreator >> cloneRepository [
	IceGitClone new
		location: self locationToUse;
		url: self remoteToUse url;
		execute.
	self repository location: self locationToUse.
		
	self commitishName 
		ifNotNil: [ self repository switchToCommitishNamed: self commitishName ].
	self repository pluginManager repositoryWillBeCreated: self repository.	

	self doEnsureProject.
	self repository workingCopy initializePackagesFromRepository.
		
	self repository pluginManager repositoryWasCreated: self repository.
	^ self repository
]

{ #category : #accessing }
IceRepositoryCreator >> commitishName [
	^ branchName
]

{ #category : #accessing }
IceRepositoryCreator >> commitishName: aName [
	branchName := aName
]

{ #category : #actions }
IceRepositoryCreator >> createNewRepositoryNamed: aString [
	| locationToUse |
	
	self validate.

	locationToUse := self locationToUse.
	(locationToUse exists 
		and: [ locationToUse hasChildren ])
		ifTrue: [ IceError signal: ('{1} is not empty.' format: {locationToUse fullName}) ].
		
	locationToUse ensureCreateDirectory.
	self subdirectory  
		ifNotEmpty: [ (locationToUse resolve: self subdirectory) ensureCreateDirectory ].

	repository := IceLibgitRepository new
		name: aString;
		location: locationToUse;
		init;
		yourself.
	
	self addProjectFileTo: repository.
	self addPropertiesFileTo: repository.
	repository workingCopy refreshProject.

	^ repository
]

{ #category : #actions }
IceRepositoryCreator >> createRepository [	
	^ [
		self validate.
		self isCloning
			ifTrue: [ self cloneRepository ]
			ifFalse: [ self addLocalRepository ] ]
	on: IceDuplicatedRepository 
	do: [ :e | 
		"This is a HACK to allow process to recover from here"
		e signal ]
]

{ #category : #'private accessing' }
IceRepositoryCreator >> defaultLocation [
	| remoteToUse |
	remoteToUse := self remoteToUse.
	^ IceLibgitRepository repositoriesLocation 
		/ remoteToUse organizer 
		/ remoteToUse projectName
]

{ #category : #private }
IceRepositoryCreator >> doEnsureProject [

	ensureProjectFile ifFalse: [ ^ self ].
	self repository project isUnborn ifFalse: [ ^ self ].

	self repository workingCopy project: (IceBasicProject onRepository: self repository withSourceDirectory: self subdirectory).
	self repository workingCopy refreshRepositoryProperties.
	
	"If the properties file has no format, set it the default legacy format"
	self repository workingCopy properties properties
		at: #format
		ifAbsent: [ 	self repository workingCopy properties fileFormat: (TIceRepositoryWriter users
			detect: #isLegacyDefault
			ifNone: [ Smalltalk at: #IceLibgitFiletreeWriter ]) ].
		
	self repository workingCopy refreshPackages.
]

{ #category : #'API-project' }
IceRepositoryCreator >> ensureProjectFile [
	ensureProjectFile := true
]

{ #category : #accessing }
IceRepositoryCreator >> formatToUse [ 
	^ IceLibgitRepository defaultFileFormat
]

{ #category : #initialization }
IceRepositoryCreator >> initialize [
	super initialize.
	ensureProjectFile := false
]

{ #category : #'private testing' }
IceRepositoryCreator >> isCloning [
	^ self remoteToUse notNil
]

{ #category : #accessing }
IceRepositoryCreator >> location [
	^ location
]

{ #category : #accessing }
IceRepositoryCreator >> location: anObject [
	location := anObject
]

{ #category : #'private accessing' }
IceRepositoryCreator >> locationToUse [
	^ self location ifNil: [ self defaultLocation ]
]

{ #category : #accessing }
IceRepositoryCreator >> remote [
	^ remote
]

{ #category : #accessing }
IceRepositoryCreator >> remote: anObject [
	remote := anObject
]

{ #category : #'private accessing' }
IceRepositoryCreator >> remoteToUse [
	self remote ifNotNil: [ ^ self remote ].
	self url ifNotNil: [ ^ IceGitRemote url: self url ]. 
	^ nil
]

{ #category : #accessing }
IceRepositoryCreator >> repository [

	^ repository ifNil: [ repository := IceLibgitRepository new ]
]

{ #category : #accessing }
IceRepositoryCreator >> repository: anExistingRepository [

	repository := anExistingRepository
]

{ #category : #accessing }
IceRepositoryCreator >> subdirectory [
	^ subdirectory ifNil: [ self class defaultSubdirectory ]
]

{ #category : #accessing }
IceRepositoryCreator >> subdirectory: anObject [
	subdirectory := anObject
]

{ #category : #accessing }
IceRepositoryCreator >> url [
	^ url
]

{ #category : #accessing }
IceRepositoryCreator >> url: anObject [
	url := anObject
]

{ #category : #private }
IceRepositoryCreator >> validate [
	self validateLocation.
	self validateAlreadyPresentAndNotGit.
	self validateNotDuplicated
]

{ #category : #private }
IceRepositoryCreator >> validateAlreadyPresentAndNotGit [
	"Validate if I will add a local repository (url isNil and location isNotNil)... 
	 testing the local directory actually exists"	
	(self remoteToUse isNil
		and: [ self location notNil
		and: [ self location exists
		and: [ (self class isGitRoot: self location) not ] ] ])
		ifTrue: [ 
			IceError signal: ('"{1}" is not a git directory' format: {self location fullName}) ]

]

{ #category : #private }
IceRepositoryCreator >> validateLocation [
	(self location isNil 
		and: [ self remoteToUse isNil ])
		ifTrue: [ IceError signal: 'No location/url or remote defined.' ]
]

{ #category : #private }
IceRepositoryCreator >> validateNotDuplicated [
	(self remoteToUse isNotNil
		and: [ self locationToUse exists and: [ self locationToUse hasChildren ] ])
		ifTrue: [ 
			IceDuplicatedRepository signal:
					('You already have an Iceberg repository 
at {1}. 

If you really want create another one, 
please locate it in a different directory.'
						format: {self locationToUse pathString}) ]
]
