Class {
	#name : #MetacelloLoadingMCSpecLoader,
	#superclass : #MetacelloCommonMCSpecLoader,
	#category : #'Metacello-MC-Loaders'
}

{ #category : #accessing }
MetacelloLoadingMCSpecLoader >> actionLabel [

	^'Loading '
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> copySpec: pkgSpec from: repositorySpecs to: repository [

	| gofer |
	gofer := MetacelloGofer new.
	(self repositoriesFrom: repositorySpecs) do: [:repo | gofer repository: repo ].
	^self copySpec: pkgSpec with: gofer to: repository
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> copySpec: pkgSpec with: gofer to: aRepository [

	| repository resolvedReference |
	[resolvedReference := pkgSpec goferLoaderReference resolveWith: gofer]
		on: Error
		do: [:ignored | ^nil ].
	repository := MCRepositoryGroup default repositories
		detect: [ :each | each = aRepository ]
		ifNone: [ aRepository ].
	repository storeVersion: resolvedReference version.
	^resolvedReference repository
]

{ #category : #actions }
MetacelloLoadingMCSpecLoader >> doLoad [
	"NOOP"
]

{ #category : #accessing }
MetacelloLoadingMCSpecLoader >> fetchingSpecLoader [

	^(MetacelloFetchingMCSpecLoader on: self spec)
		shouldDisablePackageCache: self shouldDisablePackageCache;
		loaderPolicy: self loaderPolicy copy;
		yourself
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> goferCommitBranchPackage: branchName using: repositorySpecs commitMessage: commitMessage [
	| gofer repoSpecs wc |
	repoSpecs := self spec repositorySpecs notEmpty
		ifTrue: [ self spec repositorySpecs ]
		ifFalse: [ repositorySpecs ].
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	wc := self spec workingCopy.
	repositorySpecs
		do: [ :repoSpec | 
			| repo |
			repo := repoSpec createRepository.
			(wc possiblyNewerVersionsIn: repo) notEmpty
				ifTrue: [ 
					self
						notify:
							'There are possibly newer versions of the package ' , self spec name printString , ' in the repository '
								, repo description printString
								, '. Cancel and manually merge if you want to pick up the changes from the later version.' ].
			gofer repository: repo ].
	gofer package: self spec name.
	[ gofer interactiveCommit ]
		on: MCVersionNameAndMessageRequest
		do: [ :ex | 
			| ref |
			ref := GoferVersionReference name: ex suggestedName.
			ex
				resume:
					{(ref packageName , '.' , branchName , '-' , ref author , '.' , ref versionNumber printString).
					commitMessage} ].
	^ true
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> goferCommitPackageUsing: repositorySpecs commitMessage: commitMessage [

	| gofer repoSpecs wc |
	repoSpecs := self spec repositorySpecs notEmpty
				ifTrue: [ self spec repositorySpecs ]
				ifFalse: [ repositorySpecs ].
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	wc := self spec workingCopy.
	repositorySpecs do: [:repoSpec | | repo |
		repo := repoSpec createRepository.
		(wc possiblyNewerVersionsIn: repo) notEmpty
			ifTrue: [ self notify: 'There are possibly newer versions of the package ', self spec name printString, ' in the repository ', repo description printString, '. Cancel and manually merge if you want to pick up the changes from the later version.' ].
		gofer repository: repo ].
	gofer package: self spec name.
	gofer commit: commitMessage.
	^true
]

{ #category : #packages }
MetacelloLoadingMCSpecLoader >> latestPackage: aString fromRepository: repositorySpecs [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	(self repositoriesFrom: repositorySpecs) do: [:repo | gofer repository: repo ].
	^([(GoferPackageReference name: aString) resolveWith: gofer]
		on: Error
		do: [:ignored | ^ nil ]) name
]

{ #category : #private }
MetacelloLoadingMCSpecLoader >> linearLoadPackageSpec: packageSpec gofer: gofer [
	MetacelloPlatform current
		do: [ | loadBlock goferLoad answers resolvedReference repo |
			resolvedReference := self
				resolvePackageSpec: packageSpec
				gofer: gofer.
			resolvedReference
				ifNil: [ "Package version already loaded into image" ^ self ].
			loadBlock := [ self preLoad: packageSpec.
			goferLoad := MetacelloGoferLoad on: MetacelloGofer new.
			goferLoad addResolved: resolvedReference.
			goferLoad execute.
			MetacelloPlatform current clearCurrentVersionCache.
			self postLoad: packageSpec ].
			(answers := packageSpec answers) notEmpty
				ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
				ifFalse: [ loadBlock value ].
			repo := resolvedReference repository.
			self hasRepositoryOverrides
				ifTrue: [ repo := self loaderPolicy repositoryMap
						at: resolvedReference name
						ifAbsent: [ resolvedReference repository ].
					resolvedReference workingCopy repositoryGroup addRepository: repo ]
				ifFalse: [ resolvedReference workingCopy repositoryGroup
						addRepository: resolvedReference repository ].
			MetacelloNotification signal:
					'Loaded -> ' , resolvedReference name , ' --- '
						, repo repositoryDescription , ' --- '
						, resolvedReference repository description ]
		displaying: 'Loading ' , packageSpec file
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> packagesNeedSavingUsing: repositorySpecs into: aCollection [

	| wc repoSpecs repo |
	(wc := self spec workingCopy) == nil ifTrue: [ ^self ].
	(wc ancestry ancestors notEmpty and: [ wc modified not])
		ifTrue: [ ^self ].
	repoSpecs := self spec repositorySpecs notEmpty
				ifTrue: [ self spec repositorySpecs ]
				ifFalse: [ repositorySpecs ].
	repo := (self spec getFile == nil or: [ wc ancestry ancestors isEmpty ])
		ifTrue: [ (self repositoriesFrom: repoSpecs ignoreOverrides: true) first ]
		ifFalse: [
			([ self resolveSpec: self spec from: repoSpecs ] 
				on: Error 
				do: [:ignored | ^self ]) repository ].
	aCollection add: self spec -> repo
]

{ #category : #doits }
MetacelloLoadingMCSpecLoader >> postLoad: packageOrVersionSpec [

	| block |
	(block := packageOrVersionSpec postLoadDoItBlock) ~~ nil
		ifTrue: [ block valueWithPossibleArgs: { self. packageOrVersionSpec. } ]
]

{ #category : #doits }
MetacelloLoadingMCSpecLoader >> preLoad: packageOrVersionSpec [

	| block |
	(block := packageOrVersionSpec preLoadDoItBlock) ~~ nil
		ifTrue: [ block valueWithPossibleArgs: { self. packageOrVersionSpec. } ]
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> repositoryFor: pkgSpec from: repositorySpecs [

	^([self resolveSpec: pkgSpec from: repositorySpecs]
		on: Error
		do: [:ignored | ^nil ]) repository
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> repositoryFor: pkgSpec with: gofer [

	^([self resolveSpec: pkgSpec with: gofer]
		on: Error
		do: [:ignored | ^nil ]) repository
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> resolveSpec: pkgSpec from: repositorySpecs [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	(self repositoriesFrom: repositorySpecs ignoreOverrides: true) 
		do: [:repo | gofer repository: repo ].
	^self resolveSpec: pkgSpec with: gofer
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> resolveSpec: pkgSpec with: gofer [

	^pkgSpec goferLoaderReference resolveWith: gofer
]

{ #category : #'development support' }
MetacelloLoadingMCSpecLoader >> savePackageUsing: repositorySpecs [

	| wc repo repoSpecs newVersion |
	(wc := self spec workingCopy) == nil ifTrue: [ ^false ].
	(wc ancestry ancestors notEmpty and: [ wc modified not])
		ifTrue: [ ^false ].
	repoSpecs := self spec repositorySpecs notEmpty
				ifTrue: [ self spec repositorySpecs ]
				ifFalse: [ repositorySpecs ].
	(self spec getFile == nil or: [ wc ancestry ancestors isEmpty ])
		ifTrue: [ repo := (self repositoriesFrom: repoSpecs ignoreOverrides: true) first ]
		ifFalse: [
			[ 	| newer |
				repo := (self resolveSpec: self spec from: repoSpecs) repository.
				newer := wc possiblyNewerVersionsIn: repo.
				newer isEmpty not 
					ifTrue: [
						(MetacelloPlatform current confirm: 'CAUTION! These versions in the repository may be newer:', 
							String cr, newer printString, String cr,
							'Do you really want to save this version?') ifFalse: [ ^false ]] ] 
				on: Error 
				do: [:ignored | ^false ] ].
	(newVersion :=  MetacelloPlatform current newVersionForWorkingCopy: wc) == nil ifTrue: [ ^false ].
	repo storeVersion: newVersion.
	^true
]
