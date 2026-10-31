"
I register the projects used by the image in Iceberg: the github projects found in the metacello registry and the Pharo repository.

The github projects are taken directly from the registry: each metacello registration knowing a github repository is described by an IcebergSystemProjectToRegister instance (one per repository, registrations of the same repository merged here) and is listed in Iceberg (with its commit and its baselines) without being cloned. The commit is resolved with the github api (logic inspired by IcePharoPlugin class>>#commitOfExternalProject:), using the version found in the github url, without any interaction with metacello.

Pharo itself is a particular case since it is loaded from the local sources of the pharo repository during the bootstrap: it is registered separately from these registrations, with the commit of the running image and all the baselines loaded from the local sources (found in the metacello registry instead of a hardcoded list, removing the dependency to IcePharoPlugin).

It is used by BaselineOfIDE at the end of the bootstrap to have Iceberg showing a viewable state of the loaded projects. Be careful: the code references Iceberg classes the loader only knows at load time, this is why all the accesses to Iceberg go through the #classNamed: method.
"
Class {
	#name : 'IcebergSystemProjectRecorder',
	#superclass : 'Object',
	#category : 'BaselineOfIDE-Baseline',
	#package : 'BaselineOfIDE',
	#tag : 'Baseline'
}

{ #category : 'execution' }
IcebergSystemProjectRecorder class >> register [

	self new register
]

{ #category : 'private' }
IcebergSystemProjectRecorder >> buildProjectsToRegister [
	"Answer the github projects of the metacello registry to register in an ordered collection: one IcebergSystemProjectToRegister per repository, merging the metacello registrations of the same repository. The projects without a github url are ignored."

	| projectsToRegister |
	projectsToRegister := OrderedCollection new.

	MetacelloProjectRegistry registry registrations do: [ :registration |
			(self projectToRegisterFromRegistration: registration) ifNotNil: [ :projectToRegister |
					(projectsToRegister
						 detect: [ :eachProject | eachProject repositoryName = projectToRegister repositoryName ]
						 ifNone: [ projectsToRegister add: projectToRegister ]) addRegistration: registration ] ].

	^ projectsToRegister
]

{ #category : 'private' }
IcebergSystemProjectRecorder >> classNamed: aSymbol [
	
	^ self class environment classNamed: aSymbol
]

{ #category : 'private' }
IcebergSystemProjectRecorder >> projectToRegisterFromGitHubDescription: aGitHubDescription [
	"Parse aGitHubDescription, a url of the form github://owner/project[:version][/directory], and answer the project to register it describes, or nil when it is not a github url, when the url is not complete enough or when its fragments are empty.
	When the url has no version, master is used since it is the master version that is loaded."

	| urlFragments ownerRepositoryName projectFragment shortRepositoryName sourceDirectory version |
	(aGitHubDescription beginsWith: 'github://') ifFalse: [ ^ nil ].

	urlFragments := (aGitHubDescription withoutPrefix: 'github://') splitOn: $/.
	urlFragments size < 2 ifTrue: [ ^ nil ].

	ownerRepositoryName := urlFragments first.
	projectFragment := urlFragments second.
	shortRepositoryName := projectFragment copyUpTo: $:.
	version := projectFragment copyAfter: $:.
	sourceDirectory := urlFragments size > 2
		                   ifTrue: [ '/' join: (urlFragments allButFirst: 2) ]
		                   ifFalse: [ '' ].

	(ownerRepositoryName isEmpty or: [ shortRepositoryName isEmpty ]) ifTrue: [ ^ nil ].

	^ IcebergSystemProjectToRegister
		  owner: ownerRepositoryName
		  projectName: shortRepositoryName
		  sourceDirectory: sourceDirectory
		  version: version
]

{ #category : 'private' }
IcebergSystemProjectRecorder >> projectToRegisterFromRegistration: aRegistration [

	^ aRegistration repositoryDescriptions
		  detect: [ :repositoryDescription | repositoryDescription beginsWith: 'github://' ]
		  ifFound: [ :description | self projectToRegisterFromGitHubDescription: description ]
		  ifNone: [ nil ]
]

{ #category : 'api' }
IcebergSystemProjectRecorder >> register [
	"Register the projects used by the image in Iceberg: the github projects found in the metacello registry, and Pharo itself (see #registerPharoRepository). The github projects are not cloned: for each project a synthetic repository is created and declared with the commit of the loaded version and the packages of its baselines."

	self registerPharoRepository.

	self buildProjectsToRegister do: [ :projectToRegister |
			self
				registerRepositoryNamed: projectToRegister projectName
				commit: projectToRegister commitSha
				baselines: projectToRegister baselines
				sourceDirectory: projectToRegister sourceDirectory ]
]

{ #category : 'private' }
IcebergSystemProjectRecorder >> registerPharoRepository [
	"Register the repository of the running image in Iceberg. During the bootstrap Pharo is loaded from the local sources of the pharo repository: all its baselines are the metacello registrations pointing to the local sources url and its commit is the commit of the running image"

	| pharoSourceRepositoryUrl pharoBaselineNames |
	pharoSourceRepositoryUrl := MetacelloProjectRegistry registry
		                            registrationNamed: 'Pharo'
		                            ifPresent: [ :pharoRegistration |  pharoRegistration repositoryDescriptions detect: [ :repositoryURI | repositoryURI endsWith: '/src' ] ]
		                            ifAbsent: [ nil ].

	pharoBaselineNames := MetacelloProjectRegistry registry registrations
		                      select: [ :registration | registration repositoryDescriptions anyOne = pharoSourceRepositoryUrl ]
		                      thenCollect: [ :registration | registration className asSymbol ].

	self
		registerRepositoryNamed: 'pharo'
		commit: SystemVersion current commitHash
		baselines: pharoBaselineNames
		sourceDirectory: 'src'
]

{ #category : 'private' }
IcebergSystemProjectRecorder >> registerRepositoryNamed: aProjectName commit: aSha baselines: aBaselineCollection sourceDirectory: aSourceDirectory [
	"Register in Iceberg a project that is not cloned locally: a repository with the given name is declared in Iceberg with a reference to the given commit and all the packages of the given baselines. aSourceDirectory is the directory of the code inside the repository.
	It replicates the behavior of IcePharoPlugin>>addProjectNamed:commit:baselines:tags:."

	| newRepository newSha newWorkingCopy |
	newRepository := (self classNamed: #IceLibgitRepository) new
		                 name: aProjectName;
		                 yourself.
	newSha := (self classNamed: #IceUnknownCommit) new
		          repository: newRepository;
		          id: aSha;
		          yourself.
	newWorkingCopy := (self classNamed: #IceWorkingCopy) basicNew
		                  repository: newRepository;
		                  initialize;
		                  referenceCommit: newSha;
		                  project: (((self classNamed: #IceBasicProject) onRepository: newRepository)
				                   sourceDirectory: aSourceDirectory;
				                   tags: #( #system );
				                   yourself);
		                  yourself.

	newRepository workingCopy: newWorkingCopy.
	(aBaselineCollection flatCollect: [ :baseline | (self classNamed: baseline) allPackageNames copyWith: baseline ] as: Set) do: [ :packageName |
		newWorkingCopy basicAddPackage: ((self classNamed: #IcePackage) named: packageName workingCopy: newWorkingCopy) ].

	newRepository register
]
