Class {
	#name : #MetacelloPackageSpec,
	#superclass : #MetacelloAbstractPackageSpec,
	#instVars : [
		'file',
		'repositories',
		'goferPackage',
		'preLoadDoIt',
		'postLoadDoIt'
	],
	#category : #'Metacello-MC-Specs'
}

{ #category : #gofer }
MetacelloPackageSpec >> ancestors [

	^self goferPackage ancestors
]

{ #category : #testing }
MetacelloPackageSpec >> compareCurrentVersion: anOperator targetVersionStatus: statusIgnored using: anMCLoader [

	self 
		currentPackageLoaded: [:bool | ^bool ] 
		comparing: anOperator 
		notLoaded: [:ignored | ^false ]
		using: anMCLoader
]

{ #category : #testing }
MetacelloPackageSpec >> compareRelativeCurrentVersion: anOperator targetVersionStatus: statusIgnored using: anMCLoader [

	^self compareCurrentVersion: anOperator targetVersionStatus: statusIgnored using: anMCLoader
]

{ #category : #testing }
MetacelloPackageSpec >> compareWorkingCopyNamed: wcName using: comarisonOperator [
	| fileRef wcRef |
	fileRef := GoferResolvedReference name: self file.
	wcRef := GoferResolvedReference name: wcName.
	^ wcRef compare: fileRef using: comarisonOperator
]

{ #category : #printing }
MetacelloPackageSpec >> configMethodBodyOn: aStream hasName: hasName indent: indent [

	| hasFile hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers |
	hasFile := file ~~ nil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
	hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [self answers isEmpty ]]) not.
	hasRequiresOrIncludesOrAnswers
		ifTrue: [ 
			self 
				configMethodBodyOn: aStream 
				hasName: hasName 
				cascading: hasFile | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt
				indent: indent ].
	self 
		configMethodOn: aStream 
		for: file 
		selector: 'file: ' 
		cascading: hasName | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers 
		cascade: hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt 
		indent: indent.
	hasRepositories
		ifTrue: [ 
			(self repositorySpecs size > 1)
				ifTrue: [ 
					hasName | hasFile | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
						ifTrue: [ 
							aStream cr;
							tab: indent. ].
					aStream 
						nextPutAll: 'repositories: ['; 
						cr;
						tab: indent + 1;
						nextPutAll: 'spec'; 
						cr.
					self repositories configMethodCascadeOn: aStream indent: indent + 1.
					aStream nextPutAll: ' ]' ]
				ifFalse: [ 
					hasName | hasFile | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
						ifTrue: [ aStream cr; tab: indent ].
					self repositories configMethodCascadeOn: aStream indent: indent ].
			hasPreLoadDoIt | hasPostLoadDoIt ifTrue: [ aStream nextPut: $; ] ].
	self 
		configMethodOn: aStream 
		for: self getPreLoadDoIt 
		selector: 'preLoadDoIt: '
		cascading: hasName | hasFile | hasRepositories | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers 
		cascade: hasPostLoadDoIt 
		indent: indent.
	self 
		configMethodOn: aStream 
		for: self getPostLoadDoIt 
		selector: 'postLoadDoIt: ' 
		cascading: hasName | hasFile | hasRepositories | hasPreLoadDoIt | hasRequiresOrIncludesOrAnswers 
		cascade: false 
		indent: indent.
	aStream nextPut: $.
]

{ #category : #printing }
MetacelloPackageSpec >> configMethodCascadeOn: aStream member: aMember last: lastCascade indent: indent [

	aMember methodUpdateSelector  == #remove:
		ifTrue: [ aStream  nextPutAll: 'removePackage: ', self name printString ]
		ifFalse: [ self configShortCutMethodBodyOn: aStream member: aMember indent: indent ].
	lastCascade
		ifTrue: [ aStream nextPut: $. ]
		ifFalse: [ aStream nextPut: $;; cr ]
]

{ #category : #printing }
MetacelloPackageSpec >> configMethodOn: aStream indent: indent [

	| hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers hasFile |
	hasFile := file ~~ nil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
	hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [self answers isEmpty ]]) not.
	aStream tab: indent; nextPutAll: 'spec '.
	hasFile | hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
		ifTrue: [
			aStream cr; tab: indent + 1; nextPutAll: 'name: ', self name printString; nextPut: $;.
			self configMethodBodyOn: aStream hasName: true indent: indent + 1 ]
		ifFalse: [
			aStream nextPutAll: 'name: ', self name printString ]
]

{ #category : #printing }
MetacelloPackageSpec >> configShortCutMethodBodyOn: aStream member: aMember indent: indent [

	| hasFile hasRepositories hasPreLoadDoIt hasPostLoadDoIt hasRequiresOrIncludesOrAnswers |
	hasFile := file ~~ nil.
	hasRepositories := self repositorySpecs size > 0.
	hasPreLoadDoIt := self getPreLoadDoIt ~~ nil.
	hasPostLoadDoIt := self getPostLoadDoIt ~~ nil.
	hasRequiresOrIncludesOrAnswers := (self requires isEmpty and: [ self includes isEmpty and: [self answers isEmpty ]]) not.
	hasRepositories | hasPreLoadDoIt | hasPostLoadDoIt | hasRequiresOrIncludesOrAnswers
		ifTrue: [
			aStream 
				nextPutAll: 'package: ', self name printString, ' ';
				nextPutAll: aMember methodUpdateSelector asString, ' ['; cr.
			aStream tab: indent + 1; nextPutAll: 'spec '.
			self configMethodBodyOn: aStream hasName: false indent: indent + 2.
			aStream nextPutAll: ' ]'.
			^self ].
	aStream nextPutAll: 'package: ', self name printString.
	hasFile
		ifTrue: [ aStream nextPutAll: ' with: ', file printString ]
]

{ #category : #'development support' }
MetacelloPackageSpec >> copySpecTo: aRepositorySpec [
	"Copy current mcz file to the repository named in aRepositorySpec"

	self loader copySpec: self from: self repositorySpecs to: aRepositorySpec createRepository
]

{ #category : #testing }
MetacelloPackageSpec >> currentPackageLoaded: loadedBlock comparing: comarisonOperator notLoaded: notLoadedBlock using: anMCLoader [
  "Use currentVersionInfoFor: because it involves the loader and returns versionInfo for a planned load 
	 (atomic loaders) or currently loaded package"

  | wcName vis |
  vis := anMCLoader ancestorsFor: self.
  vis notNil
    ifTrue: [ 
      | fileRef wcRef |
      self getFile == nil
        ifTrue: [ ^ loadedBlock value: false ].
      vis
        do: [ :vi | 
          wcName := vi name.
          fileRef := GoferVersionReference name: self file.
          fileRef versionNumber = 0
            ifTrue: [ 
              "a shame that GoferVersionReference doesn't have better method for recognizing a missing verion number"
              "fix for: https://github.com/dalehenrich/metacello-work/issues/185"
              ^ loadedBlock value: false ].
          wcRef := GoferVersionReference name: wcName.
          (wcRef compare: fileRef using: comarisonOperator)
            ifTrue: [ ^ loadedBlock value: true ] ].
      ^ loadedBlock value: false ].
  ^ notLoadedBlock value: true
]

{ #category : #testing }
MetacelloPackageSpec >> currentPackageLoaded: loadedBlock notLoaded: notLoadedBlock using: anMCLoader [
	"Use currentVersionInfoFor: because it involves the loader and returns versionInfo for a planned load 
	 (atomic loaders) or currently loaded package"

	| vis |
	vis := anMCLoader ancestorsFor: self.
	vis notNil
		ifTrue: [ 
			self getFile == nil
				ifTrue: [ ^ loadedBlock value: #() value: self file ].
			^ loadedBlock value: vis value: self file ].
	^ notLoadedBlock value

]

{ #category : #gofer }
MetacelloPackageSpec >> currentVersionInfo [

	^self goferPackage currentVersionInfo
]

{ #category : #loading }
MetacelloPackageSpec >> ensureLoadUsing: mcLoader [
	self explicitLoadUsing: mcLoader ensureSpecLoader
]

{ #category : #loading }
MetacelloPackageSpec >> ensureLoadedForDevelopmentUsing: mcLoader [
	"noop"
	
	^true
]

{ #category : #loading }
MetacelloPackageSpec >> explicitLoadUsing: mcLoader [

	| wc fetchingSpecLoader |
	((wc := self workingCopy) ~~ nil and: [ wc needsSaving ]) 
		ifTrue: [ 
			(MetacelloSkipDirtyPackageLoad signal: self)
				ifTrue:  [
					MetacelloNotification signal: ('Skipping load of modified package: ', self file).
					^self]
				ifFalse: [MetacelloNotification signal: ('Load over modified package: ', self file)] ].
	"fetch and explicitly load it"
	fetchingSpecLoader := mcLoader fetchingSpecLoader.
	fetchingSpecLoader
		explicitLoadPackageSpecs: (Array with: self) 
		repositories: (fetchingSpecLoader repositoriesFrom: self repositorySpecs).
]

{ #category : #private }
MetacelloPackageSpec >> extractNameFromFile [

	file == nil ifTrue: [ ^nil ].
	^(self loader nameComponentsFrom: self file) first
]

{ #category : #loading }
MetacelloPackageSpec >> fetch [

	self fetchUsing: self loader
]

{ #category : #loading }
MetacelloPackageSpec >> fetchPackage: aLoaderPolicy [

	self fetchUsing: 
		(self loader
			loaderPolicy: aLoaderPolicy;
			yourself)
]

{ #category : #loading }
MetacelloPackageSpec >> fetchUsing: mcLoader [

	| fetchingSpecLoader |
	fetchingSpecLoader := mcLoader fetchingSpecLoader.
	fetchingSpecLoader
		linearLoadPackageSpecs: (Array with: self) 
		repositories: (fetchingSpecLoader repositoriesFrom: self repositorySpecs).
]

{ #category : #querying }
MetacelloPackageSpec >> file [

	file == nil ifTrue: [ ^self name ].
	^file
]

{ #category : #accessing }
MetacelloPackageSpec >> file: aString [

	file := aString
]

{ #category : #construction }
MetacelloPackageSpec >> file: aString constructor: aVersionConstructor [
    aVersionConstructor fileForPackage: aString
]

{ #category : #'development support' }
MetacelloPackageSpec >> forceUpdatePackageSpec: updatedSpecs using: anMCLoader [
	
	self updatePackageSpec: updatedSpecs force: true using: anMCLoader
]

{ #category : #accessing }
MetacelloPackageSpec >> getFile [
	"raw access to iv"
	
	^file
]

{ #category : #accessing }
MetacelloPackageSpec >> getName [
    "raw access to iv"

    ^ name
]

{ #category : #querying }
MetacelloPackageSpec >> getPostLoadDoIt [

	^postLoadDoIt
]

{ #category : #querying }
MetacelloPackageSpec >> getPreLoadDoIt [

	^preLoadDoIt
]

{ #category : #accessing }
MetacelloPackageSpec >> getRepositories [
    "raw access to iv"

    ^ repositories
]

{ #category : #'development support' }
MetacelloPackageSpec >> goferBranchPackage: branchName message: commitMessage [
	"uses gofer to do commit ... non-interactive"

	| latestFile pkgSpec   |
	(file notNil and: [ (self name, '.', branchName) = self file ])
		ifTrue: [ 
			latestFile := self loader latestPackage: self file fromRepository: self repositorySpecs.
			pkgSpec := self copy.
			latestFile ~~ nil
				ifTrue: [ 
					pkgSpec file: latestFile.
					pkgSpec goferCommitPackage: commitMessage.
					^ self ] ].
	self loader goferCommitBranchPackage: branchName using: self repositorySpecs commitMessage: commitMessage
]

{ #category : #'development support' }
MetacelloPackageSpec >> goferCommitPackage: commitMessage [
	"uses gofer to do commit ... non-interactive"
	
	| latestFile pkgSpec |
	^(file notNil and: [ self name = self file ])
		ifTrue: [
			latestFile := self loader latestPackage: self name fromRepository: self repositorySpecs.
			pkgSpec := self copy.
			pkgSpec file: latestFile.
			pkgSpec goferCommitPackage: commitMessage ]
		ifFalse: [ self loader goferCommitPackageUsing: self repositorySpecs commitMessage: commitMessage ]
]

{ #category : #accessing }
MetacelloPackageSpec >> goferLoaderReference [

	^file == nil 
		ifTrue: [ GoferPackageReference name: self name ]
		ifFalse: [ 
			"does Monticello-style #versionInfoFromVersionNamed: matching"
			MetacelloGoferPackage name: self name packageFilename: self file ]
]

{ #category : #accessing }
MetacelloPackageSpec >> goferPackage [

	goferPackage == nil 
		ifTrue: [ goferPackage := MetacelloGoferPackage name: self name packageFilename: self file ].
	^goferPackage
]

{ #category : #testing }
MetacelloPackageSpec >> hasRepository [
    ^ self repositorySpecs notEmpty
]

{ #category : #construction }
MetacelloPackageSpec >> includes: anObject constructor: aVersionConstructor [
    aVersionConstructor includesForPackage: anObject
]

{ #category : #private }
MetacelloPackageSpec >> includesForPackageOrdering [
  ^ self includes
]

{ #category : #accessing }
MetacelloPackageSpec >> info [
	"test compatibility method"

	^self
]

{ #category : #querying }
MetacelloPackageSpec >> isPackageLoaded [

	^self isPackageLoaded: self loader
]

{ #category : #querying }
MetacelloPackageSpec >> isPackageLoaded: aLoader [

	^(self workingCopyNameFor: aLoader) ~~ nil
]

{ #category : #loading }
MetacelloPackageSpec >> load [

	self explicitLoadUsing: self loader
]

{ #category : #loading }
MetacelloPackageSpec >> loadUsing: mcLoader [

	self loader doingLoads: [ self explicitLoadUsing: mcLoader ]
]

{ #category : #loading }
MetacelloPackageSpec >> loadUsing: aLoader gofer: gofer [

	^aLoader linearLoadPackageSpec: self gofer: gofer
]

{ #category : #merging }
MetacelloPackageSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #file put: file.
	map at: #repositories put: self repositories.
	map at: #preLoadDoIt put: preLoadDoIt.
	map at: #postLoadDoIt put: postLoadDoIt.
	^map
]

{ #category : #merging }
MetacelloPackageSpec >> mergeSpec: anotherSpec [

	| newSpec map anotherRepositories |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	(anotherRepositories := map at: #repositories) notEmpty
		ifTrue: [ 
			newSpec 
				repositories: (self repositories isEmpty
					ifTrue: [ anotherRepositories ]
					ifFalse: [ self repositories mergeSpec: anotherRepositories ]) ].
	^newSpec

]

{ #category : #querying }
MetacelloPackageSpec >> name [

	name == nil ifTrue: [ name := self extractNameFromFile ].
	^name
]

{ #category : #merging }
MetacelloPackageSpec >> nonOverridable [

	^super nonOverridable, #( repositories)
]

{ #category : #loading }
MetacelloPackageSpec >> packageSpecsInLoadOrder [

	^{ self. }
]

{ #category : #'development support' }
MetacelloPackageSpec >> packagesNeedSavingVisited: visitedProjects using: repos into: aCollection [

	^self loader packagesNeedSavingUsing: repos into: aCollection
]

{ #category : #copying }
MetacelloPackageSpec >> postCopy [

	super postCopy.
	goferPackage := nil.
	repositories := repositories copy.
]

{ #category : #querying }
MetacelloPackageSpec >> postLoadDoIt [

	^postLoadDoIt
]

{ #category : #loading }
MetacelloPackageSpec >> postLoadDoIt: anObject [

	anObject setPostLoadDoItInMetacelloSpec: self
]

{ #category : #construction }
MetacelloPackageSpec >> postLoadDoIt: aSymbol constructor: aVersionConstructor [
    aVersionConstructor postLoadDoItForPackage: aSymbol
]

{ #category : #querying }
MetacelloPackageSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : #accessing }
MetacelloPackageSpec >> preLoadDoIt: anObject [

	anObject setPreLoadDoItInMetacelloSpec: self
]

{ #category : #construction }
MetacelloPackageSpec >> preLoadDoIt: aSymbol constructor: aVersionConstructor [
    aVersionConstructor preLoadDoItForPackage: aSymbol
]

{ #category : #visiting }
MetacelloPackageSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	packageBlock value: self
]

{ #category : #querying }
MetacelloPackageSpec >> repositories [

	repositories == nil ifTrue: [ repositories := self project repositoriesSpec ].
	^ repositories
]

{ #category : #querying }
MetacelloPackageSpec >> repositories: anObject [
	repositories := anObject
]

{ #category : #construction }
MetacelloPackageSpec >> repositories: aBlock constructor: aVersionConstructor [
    aVersionConstructor repositoriesForPackage: aBlock
]

{ #category : #querying }
MetacelloPackageSpec >> repository [
    self deprecated: 'Use repositories or repositorySpecs'.
    self repositorySpecs isEmpty
        ifTrue: [ ^ nil ].
    ^ self repositorySpecs first
]

{ #category : #accessing }
MetacelloPackageSpec >> repository: aStringOrMetacelloRepositorySpec [

	self repositories repository: aStringOrMetacelloRepositorySpec
]

{ #category : #construction }
MetacelloPackageSpec >> repository: anObject constructor: aVersionConstructor [
    aVersionConstructor repositoryForPackage: anObject
]

{ #category : #accessing }
MetacelloPackageSpec >> repository: aString username: username password: password [

	self repositories repository: aString username: username password: password
]

{ #category : #construction }
MetacelloPackageSpec >> repository: aString username: username password: password constructor: aVersionConstructor [
    aVersionConstructor repositoryForPackage: aString username: username password: password
]

{ #category : #loading }
MetacelloPackageSpec >> repositoryDescriptions [
    ^ self repositorySpecs collect: [ :repoSpec | repoSpec description ]
]

{ #category : #loading }
MetacelloPackageSpec >> repositorySpecs [

	^self repositories map values
]

{ #category : #construction }
MetacelloPackageSpec >> requires: anObject constructor: aVersionConstructor [
    aVersionConstructor requiresForPackage: anObject
]

{ #category : #private }
MetacelloPackageSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	^{ self }
]

{ #category : #'development support' }
MetacelloPackageSpec >> savePackage [
	"Interactive save ... prompted for commit message and package name unless MCVersionNameAndMessageRequest handled"

	| latestFile pkgSpec |
	^(file notNil and: [ self name = self file ])
		ifTrue: [
			latestFile := self loader latestPackage: self name fromRepository: self repositorySpecs.
			pkgSpec := self copy.
			pkgSpec file: latestFile.
			pkgSpec savePackage ]
		ifFalse: [ self loader savePackageUsing: self repositorySpecs ]
]

{ #category : #fetching }
MetacelloPackageSpec >> searchCacheRepositoryForPackage: searchBlock [
  "evaluate the <searchBlock> if you want to search for the package in a local package cache"

  "for standard mcz repositories the answer is YES!"

  searchBlock value
]

{ #category : #accessing }
MetacelloPackageSpec >> setPostLoadDoIt: aSymbol [

	postLoadDoIt := aSymbol
]

{ #category : #accessing }
MetacelloPackageSpec >> setPreLoadDoIt: aSymbol [

	preLoadDoIt := aSymbol
]

{ #category : #construction }
MetacelloPackageSpec >> supplyingAnswers: anObject constructor: aVersionConstructor [
    aVersionConstructor supplyingAnswersForPackage: anObject
]

{ #category : #'development support' }
MetacelloPackageSpec >> updateForSpawnMethod: sourceSpec [
    "This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"

    | nm fl |
    nm := name.
    fl := file.
    fl == nil
        ifTrue: [ 
            "if only name has been set, then force the file to be non-nil, if any attribute besides file is set, then leave file nil"
            {answers.
            requires.
            includes.
            repositories.
            preLoadDoIt.
            postLoadDoIt} detect: [ :each | each ~~ nil ] ifNone: [ fl := name ] ].
    super updateForSpawnMethod: sourceSpec.
    file := repositories := goferPackage := preLoadDoIt := postLoadDoIt := nil.
    name := nm.
    file := fl.
    ^ file == nil
]

{ #category : #'development support' }
MetacelloPackageSpec >> updatePackageRepositories: repositorySpecs [

	| resolvedPackageRef |
	MetacelloNotification signal: ('  Looking up version -> ', self file).
	resolvedPackageRef := self loader resolveSpec: self from: repositorySpecs.
	MetacelloNotification signal: ('Update repositoryGroup -> ', resolvedPackageRef name, ' ' , resolvedPackageRef repository description).
	resolvedPackageRef version workingCopy repositoryGroup addRepository: resolvedPackageRef repository
]

{ #category : #'development support' }
MetacelloPackageSpec >> updatePackageRepositoriesFor: aVersionSpec [
	"Don't update the repository unless the package is loaded in the image"
	
	self workingCopy == nil ifTrue: [ ^self ].
	self updatePackageRepositories: self repositorySpecs, aVersionSpec repositorySpecs.
]

{ #category : #'development support' }
MetacelloPackageSpec >> updatePackageSpec: updatedSpecs force: force using: anMCLoader [
	"Add pkg copy to updatedSpecs if the file in current image is different from the receiver's file"

	| viName |
	(force not and: [ self getFile == nil ])
		ifTrue: [ ^ self ].	"no file explicitly specified in this spec"
	(viName := self workingCopyNameFor: anMCLoader) == nil
		ifTrue: [ ^ self ].	"no working copy"
	viName ~= self file
		ifTrue: [ 
			| spec |
			spec := self copy.
			spec file: viName.
			updatedSpecs at: spec name put: spec ]
		ifFalse: [ updatedSpecs at: self name put: #uptodate ]
]

{ #category : #'development support' }
MetacelloPackageSpec >> updatePackageSpec: updatedSpecs using: anMCLoader [
	"Add pkg copy to updatedSpecs if the file in current image is different from the receiver's file"
	
	self updatePackageSpec: updatedSpecs force: false using: anMCLoader
]

{ #category : #visiting }
MetacelloPackageSpec >> visitingWithPackages: packages [

	packages at: self name put: self
]

{ #category : #gofer }
MetacelloPackageSpec >> workingCopy [

	^self goferPackage workingCopy
]

{ #category : #gofer }
MetacelloPackageSpec >> workingCopyName [

	| wc |
	(wc := self workingCopy) == nil ifTrue: [ ^nil ].
	wc ancestry ancestors isEmpty not
		ifTrue: [ ^wc ancestry ancestors first name ].
	^nil
]

{ #category : #gofer }
MetacelloPackageSpec >> workingCopyNameFor: anMCLoader [

	| vi |
	(vi := anMCLoader currentVersionInfoFor: self) == nil ifTrue: [ ^nil ].
	^vi name
]
