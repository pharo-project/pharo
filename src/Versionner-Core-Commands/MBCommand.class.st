"
MBCommand is a command design pattern for capturing actions.
"
Class {
	#name : #MBCommand,
	#superclass : #Object,
	#instVars : [
		'target',
		'targetNode',
		'requestor'
	],
	#category : #'Versionner-Core-Commands'
}

{ #category : #testing }
MBCommand class >> isMorphicCommand [

	^true
]

{ #category : #testing }
MBCommand class >> isOBCommand [

	^true
]

{ #category : #util }
MBCommand class >> newCommand [
	" 
	MBCommand newCommand
	"
	| command baseName newCommandName receiverClassIndex clss title documentation |
	baseName :=  self uiManager request: 'Enter the base name (''MB'' and ''Command'' will be automatically added'.
	baseName ifNil: [ ^ self ].
	
	clss := MBCommand subclasses.
	receiverClassIndex := self uiManager chooseFrom: (clss collect: #name).
	receiverClassIndex isZero ifTrue: [ ^ self ].
	
	newCommandName := 'MB', baseName, 'Command'.
	command := 
		(clss at: receiverClassIndex) 
			subclass: newCommandName asSymbol 
			instanceVariableNames: '' 
			classVariableNames: '' 
			poolDictionaries: '' 
			category: self category.
	
	documentation := self uiManager request: 'Enter the documentation'.
	documentation ifNil: [ ^ self ].
	 command compile: 'documentation
	^ ''', documentation, '''' classified: 'documentation'.

	 command compile: 'execute
	self subclassResponsibility ' classified: 'execute'.
	
	title := self uiManager request: 'Enter the command title'.
	title ifNil: [ ^ self ].
	 command compile: 'title
	^ ''', title, '''' classified: 'accessing'.
	
	clss := MBInfo allSubclasses.
	receiverClassIndex := self uiManager chooseFrom: (clss collect: #name).
	receiverClassIndex isZero ifTrue: [ ^ self ].
	(clss at: receiverClassIndex) compile: 'cmd', baseName, '
	^ ', newCommandName classified: 'commands'
]

{ #category : #'instance creation' }
MBCommand class >> target: target for: requestor [
	^ self new
		target: target;
		requestor: requestor;
		yourself
]

{ #category : #versions }
MBCommand >> askForVersionNumber [
	"Return a new version number as a string"

	| latestVersion newVersion versions |
	newVersion := (versions := target configurationInfo versions) isEmpty
		ifTrue: [ '1.0' ]
		ifFalse: [ 
			latestVersion := versions first version.
			latestVersion versionNumber copy incrementMinorVersionNumber printString ].
	^ self uiManager  request: 'enter new version number' initialAnswer: newVersion
]

{ #category : #versions }
MBCommand >> browseVersionHistory [
	"Used to browse version history of a Configuration package or a package"
	
	target workingCopy ancestors isEmpty
		ifTrue: [ ^ self ].
	(MCVersionHistoryBrowser new ancestry: target workingCopy ancestors first) perform: #show
]

{ #category : #testing }
MBCommand >> canBeApplied [
	^ true
]

{ #category : #repositories }
MBCommand >> checkUserInRepository: repository [
	"Try to find the http repository of packageName. If it does not find one, then add one and ask for the username and password"

	| answerUser |
	(repository isKindOf: MCHttpRepository ) ifFalse: [ ^self ].
	repository user
		ifEmpty: [ 
			answerUser := self uiManager confirm: 'No user defined. Do you want to add one? '.
			answerUser
				ifTrue: [ 
					repository user: self username.
					repository password: self password ] ]
]

{ #category : #repositories }
MBCommand >> checkUserInRepositoryOfPackage: packageName [
	"Try to find the http repository of packageName. If it does not find one, then add one and ask for the username and password"

	| repo |
	(repo := self repositoryOfPackageNamed: packageName) ifNil: [ ^nil ].
	self checkUserInRepository: repo.
	^repo
]

{ #category : #utility }
MBCommand >> chooseRepositoryFromList: repos [
	"Return the repository of the configuration. Create one (using UI) if necessary"

	repos size > 1
		ifTrue: [ 
			^ self uiManager
				chooseFrom: (repos collect: [ :repo | repo description ])
				values: repos
				title: 'Select repository' ].
	^ repos first
]

{ #category : #versions }
MBCommand >> compiledMethodForVersion: aMetacelloVersion [

	| toolbox pragma |
	toolbox := MetacelloToolBox new project: aMetacelloVersion project.
	pragma := (toolbox constructor extractAllVersionPragmas at: aMetacelloVersion versionString ifAbsent: [ ^ nil ]) first.
	^pragma method
]

{ #category : #'accessing-computed' }
MBCommand >> configurationClass [
	^ target
]

{ #category : #repositories }
MBCommand >> configurationReferencesFrom: aRepository [

	| map |
	map := Dictionary new.
	aRepository goferReferences do: [ :ref | 
		(ref packageName beginsWith: 'ConfigurationOf')
			ifTrue: [ | nameWithBranch |
				nameWithBranch := ref metacelloPackageNameWithBranch at: 2.
				(map at: nameWithBranch ifAbsent: [map at: nameWithBranch put: OrderedCollection new]) add: ref ]].
	^map
]

{ #category : #documentation }
MBCommand >> documentation [
	self subclassResponsibility
]

{ #category : #execute }
MBCommand >> execute [
	self subclassResponsibility 
]

{ #category : #execute }
MBCommand >> executeWithDevWorkflow: devWorkflow [
	self execute
]

{ #category : #changes }
MBCommand >> findChangesBetween: versionSnapshot toLabel: toLabel and: ancestorVersionSnapshot fromLabel: fromLabel [
	"View changes between 2 versions."
	| patch |
	
	patch := versionSnapshot patchRelativeToBase: ancestorVersionSnapshot.
	patch isNil ifTrue: [^ self].
	patch isEmpty
		ifTrue: [
			self uiManager inform: 'No changes' ]
		ifFalse: [
			self
				viewChanges: patch
				from: fromLabel
				to: toLabel
				title: 'Changes between ', fromLabel, ' and ', toLabel   ]
]

{ #category : #changes }
MBCommand >> findChangesFor: workingCopy relativeToRepository: repository [
	| patch fromDescription |
	repository isNil
		ifTrue: [ ^ self ].
	patch := workingCopy changesRelativeToRepository: repository.
	patch isNil
		ifTrue: [ ^ self ].
	patch isEmpty
		ifTrue: [ 
			workingCopy modified: false.
			self uiManager inform: 'No changes' ]
		ifFalse: [ 
			workingCopy modified: true.
			fromDescription := workingCopy packageName , ' (' , workingCopy ancestry ancestorString , ')'.
			self
				viewChanges: patch
				from: fromDescription
				to: ('Modified {1}' translated format: {(workingCopy description)}) 
				title: ('Changes to {1}' translated format: {fromDescription}) ]
]

{ #category : #documentation }
MBCommand >> helpDocumentation [
	^self documentation
]

{ #category : #'initialize-release' }
MBCommand >> initialize [
	super initialize.

]

{ #category : #utility }
MBCommand >> on: anObject for: aRequestor [
	^ self class on: anObject for: aRequestor
]

{ #category : #ordering }
MBCommand >> order [
	^ self title
]

{ #category : #repositories }
MBCommand >> password [
	| password |
	password := target configurationRoot password. 
	password ifNotNil: [ ^ password ].
	password := self uiManager requestPassword: 'enter your Monticello password for '.
	target configurationRoot password: password.
	^ password
]

{ #category : #repositories }
MBCommand >> pickRepository [
	^ self pickRepositoryFrom: self repositories.

]

{ #category : #repositories }
MBCommand >> pickRepositoryFrom: repositories [

	^self pickRepositoryFrom: repositories title: 'Choose repository'
]

{ #category : #repositories }
MBCommand >> pickRepositoryFrom: repositories title: title [
	| index |
	index := self uiManager chooseFrom: (repositories collect: [ :ea | ea description ]) title: title.
	^ index = 0
		ifFalse: [ repositories at: index ] 
]

{ #category : #'accessing-computed' }
MBCommand >> projectName [
	^ self configurationClass name configurationBaseName
]

{ #category : #repositories }
MBCommand >> repositories [
	^MCRepositoryGroup default repositories
]

{ #category : #repositories }
MBCommand >> repositoryOfPackageNamed: packageName [
	"Return the repository of the configuration. Create one (using UI) if necessary"

	| mcPackage workingCopy repositories repository repositoryIndex username password |
	mcPackage := MCPackage named: packageName.
	workingCopy := mcPackage workingCopy.
	repositories := workingCopy repositoryGroup repositories reject: [ :rep | rep == MCCacheRepository uniqueInstance ].
	repositories
		ifEmpty: [ 
			| projectName answer squeakSourceURL |
			projectName := self configurationClass name configurationBaseName.
			squeakSourceURL := 'http://www.squeaksource.com/' , projectName.
			answer := self uiManager
				confirm: 'No repository associated with your project. Do you want to add ' , squeakSourceURL.
			answer
				ifFalse: [ 
					self uiManager  inform: 'Add a http repository with Monticello browser'.
					^ nil ].
			repository := MCHttpRepository location: squeakSourceURL user: '' password: ''.
			repository user isEmpty
				ifTrue: [ username := self username ].
			repository password isEmpty
				ifTrue: [ password := self password ].
			workingCopy repositoryGroup
				addRepository: (repository := MCHttpRepository location: squeakSourceURL user: username password: password) ].
	repositories size > 1
		ifTrue: [ 
			repositoryIndex := self uiManager chooseFrom: (repositories collect: #printString).
			repositoryIndex = 0
				ifTrue: [ ^ nil ].
			repository := repositories at: repositoryIndex ].
	repositories size = 1
		ifTrue: [ repository := repositories first ].
	^ repository
]

{ #category : #accessing }
MBCommand >> requestor: aRequestor [

	requestor := aRequestor 
]

{ #category : #utility }
MBCommand >> selectRepositoryFromWorkingCopy: workingCopy [
	| repos repo |
	repos := workingCopy repositoryGroup repositories.
	repos size = 0
		ifTrue: [ ^ self uiManager  inform: 'No repositories associated with ' , target name printString ].
	repo := repos size = 1
		ifTrue: [ repos first ]
		ifFalse: [ 
			self uiManager 
				chooseFrom: (repos collect: [ :r | r description ])
				values: repos
				title: 'Please choose which repository to use for diff' ].
	^ repo
]

{ #category : #accessing }
MBCommand >> target [
	^ target
]

{ #category : #accessing }
MBCommand >> target: anObject [
	target := anObject
]

{ #category : #accessing }
MBCommand >> targetNode [
	^ targetNode
]

{ #category : #accessing }
MBCommand >> targetNode: anObject [
	targetNode := anObject
]

{ #category : #accessing }
MBCommand >> title [
	^ 'Abstract Command'
]

{ #category : #utility }
MBCommand >> uiManager [

	^ UIManager default
]

{ #category : #repositories }
MBCommand >> username [
	| username |
	username := target configurationRoot username. 
	username ifNotNil: [ ^ username ].
	username := self uiManager request: 'enter your Monticello username (login)'.
	target configurationRoot username: username.
	^ username
]

{ #category : #changes }
MBCommand >> viewChanges: patch from: fromDescription to: toDescription title: title [
	"Open a browser on the given patch."

	^ ( PSMCPatchMorph forPatch:  patch)
			fromDescription: fromDescription;
			toDescription:  toDescription;
			newWindow;
			title: title;
			open 
]
