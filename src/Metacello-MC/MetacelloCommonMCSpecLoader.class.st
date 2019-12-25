Class {
	#name : #MetacelloCommonMCSpecLoader,
	#superclass : #MetacelloSpecLoader,
	#instVars : [
		'operator',
		'loaderPolicy',
		'disablePackageCache'
	],
	#classVars : [
		'RetryPackageResolution'
	],
	#category : #'Metacello-MC-Loaders'
}

{ #category : #utilities }
MetacelloCommonMCSpecLoader class >> nameComponentsFrom: aVersionName [
	| ar |
	ar := (aVersionName last isDigit and: [ (aVersionName indexOf: $.) > 0 ])ifFalse: [Array with: aVersionName with: '' with: 0 with: aVersionName with: self]
		ifTrue: [ | vrsn str |
			str := ((aVersionName copyAfterLast: $-) copyAfterLast: $.).
			vrsn := str isEmpty
				ifTrue: [0]
				ifFalse: [str asInteger].
			Array
				with: (aVersionName copyUpToLast:  $-)								"base pkg name"
				with: ((aVersionName copyAfterLast: $-) copyUpTo: $.)				"user"
				with:  vrsn "version"
				with: aVersionName
				with: self ].
	^ar
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader class >> retryPackageResolution [
    "if true, 
		errors during #retryingResolvePackageSpecReferences:gofer: are caught and 
		the resolution is retried 3 times. After the thrid time, a MetacelloPackageSpecResolutionError
		is thrown
	if false,
		an error during #retryingResolvePackageSpecReferences:gofer: will be passed,
		likely resulting in a walkback ... useful for debugging."

    RetryPackageResolution ifNil: [ RetryPackageResolution := true ].
    ^ RetryPackageResolution
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader class >> retryPackageResolution: aBool [
    RetryPackageResolution := aBool
]

{ #category : #versionInfo }
MetacelloCommonMCSpecLoader >> ancestorsFor: packageSpec [

	| cacheKey vi |
	cacheKey := packageSpec file.
	^MetacelloPlatform current
		stackCacheFor: #ancestors
		at: cacheKey
		doing: [ :cache | 
			vi := packageSpec ancestors.
			cache at: cacheKey put: vi ].
]

{ #category : #versionInfo }
MetacelloCommonMCSpecLoader >> currentVersionInfoFor: packageSpec [

	| cacheKey vi |
	cacheKey := packageSpec file.
	^MetacelloPlatform current
		stackCacheFor: #currentVersionInfo
		at: cacheKey
		doing: [ :cache | 
			vi := packageSpec currentVersionInfo.
			cache at: cacheKey put: vi ].
]

{ #category : #actions }
MetacelloCommonMCSpecLoader >> doLoad [

	self subclassResponsibility
]

{ #category : #actions }
MetacelloCommonMCSpecLoader >> doingLoads: aBlock [
	"escape mechanism for recording and null loaders to skip doing loaderlike things"
	
	aBlock value
]

{ #category : #testing }
MetacelloCommonMCSpecLoader >> ensureForDevelopment [
  ^ true
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> ensureSpecLoader [
	^ self
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> fetchingSpecLoader [

	^self
]

{ #category : #testing }
MetacelloCommonMCSpecLoader >> hasRepositoryOverrides [

	^self loaderPolicy hasRepositoryOverrides
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> ignoreImage [

	^self loaderPolicy ignoreImage
]

{ #category : #initialization }
MetacelloCommonMCSpecLoader >> initialize [

	self loaderPolicy
]

{ #category : #actions }
MetacelloCommonMCSpecLoader >> linearLoadPackageSpec: packageSpec gofer: gofer [

	self subclassResponsibility
]

{ #category : #private }
MetacelloCommonMCSpecLoader >> linearLoadPackageSpecs: packageSpecs repositories: repositories [
	
	| gofer |
	gofer := MetacelloGofer new.
	repositories do: [:repo | gofer repository: repo ].
	packageSpecs do: [:pkg | pkg loadUsing: self gofer: gofer ].
]

{ #category : #actions }
MetacelloCommonMCSpecLoader >> load [
	| repos |
	repos := self repositoriesFrom: self spec repositorySpecs.
	^ self loadType == #atomic
		ifTrue: [self atomicLoadPackageSpecs: self spec packageSpecsInLoadOrder repositories: repos]
		ifFalse: ["assume #linear"
			self linearLoadPackageSpecs: self spec packageSpecsInLoadOrder repositories: repos ]
]

{ #category : #actions }
MetacelloCommonMCSpecLoader >> loadPackageDirective: aPackageLoadDirective gofer: aGofer [

	| packageSpec |

	packageSpec := aPackageLoadDirective spec.
	MetacelloPlatform current
		do: [ | loadBlock goferLoad answers resolvedReference |

			aGofer disablePackageCache.	"for good luck:)"
			resolvedReference := self resolvePackageSpec: packageSpec gofer: aGofer.
			resolvedReference ifNil: [ "Package version already loaded into image" ^ self ].
			loadBlock := [ "mcLoader preLoad: packageSpec."
			goferLoad := MetacelloGoferLoad on: aGofer.
			goferLoad addResolved: resolvedReference.
			goferLoad execute.
			MetacelloPlatform current clearCurrentVersionCache	"mcLoader postLoad: packageSpec"
			].
			( answers := packageSpec answers ) notEmpty
				ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
				ifFalse: [ loadBlock value ].
			resolvedReference workingCopy repositoryGroup addRepository: aPackageLoadDirective repository.
			MetacelloNotification signal:
					'Loaded -> ' , resolvedReference name , ' --- '
						, aPackageLoadDirective repository repositoryDescription , ' --- '
						, resolvedReference repository description
			]
		displaying: 'Loading ' , packageSpec file
]

{ #category : #actions }
MetacelloCommonMCSpecLoader >> loadPackageDirectives: pkgLoads gofer: aGofer [
  MetacelloPlatform current
    do: [ 
      | goferLoad loadBlock answers |
      goferLoad := MetacelloGoferLoad on: aGofer.
      answers := OrderedCollection new.
      pkgLoads
        do: [ :packageLoadDirective | 
          | resolvedReference |
          aGofer disablePackageCache.	"for good luck:)"
          (resolvedReference := self
            resolvePackageSpec: packageLoadDirective spec
            gofer: aGofer) ~~ nil
            ifTrue: [ 
              goferLoad addResolved: resolvedReference.
              answers addAll: packageLoadDirective spec answers.
              packageLoadDirective resolvedReference: resolvedReference ] ].
     MetacelloNotification signal: 'Starting atomic load'.
      loadBlock := [ 
      "pkgLoads do: [:packageLoadDirective |  mcLoader preLoad: packageLoadDirective spec ]."
      goferLoad execute.
      pkgLoads
        do: [ :packageLoadDirective | 
          packageLoadDirective resolvedReference == nil
            ifTrue: [ 
					MetacelloNotification signal: ('Already Loaded -> ' , packageLoadDirective file) level: 2]
            ifFalse: [ 
            		MetacelloNotification signal: ('Loaded -> ' , packageLoadDirective file , ' --- '
                        , packageLoadDirective repository repositoryDescription
                        , ' --- '
                        ,
                          packageLoadDirective resolvedReference repository description) level: 2.
              packageLoadDirective resolvedReference workingCopy repositoryGroup
                addRepository: packageLoadDirective repository ] ].
      MetacelloPlatform current clearCurrentVersionCache	"pkgLoads do: [:packageLoadDirective | 
					packageLoadDirective resolvedReference ~~ nil
						ifTrue: [mcLoader postLoad: packageLoadDirective spec ]]" ].
      answers notEmpty
        ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
        ifFalse: [ loadBlock value ].
      MetacelloNotification signal: 'Finished atomic load' ]
    displaying: 'Atomic Load...'
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> loaderPolicy [

	loaderPolicy == nil ifTrue: [ loaderPolicy := MetacelloLoaderPolicy new ].
	^loaderPolicy
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> loaderPolicy: anObject [
	loaderPolicy := anObject
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> loadingSpecLoader [

	^self
]

{ #category : #packages }
MetacelloCommonMCSpecLoader >> nameComponentsFrom: aVersionName [

	^self class nameComponentsFrom: aVersionName
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> operator [

	operator == nil ifTrue: [ ^#= ].
	^operator
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> operator: aSymbol [

	operator := aSymbol
]

{ #category : #doits }
MetacelloCommonMCSpecLoader >> postLoad: packageOrVersionSpec [
	"subclassResponsibility, but it gets called during an upgrade, so leave it as NOOP"
]

{ #category : #doits }
MetacelloCommonMCSpecLoader >> preLoad: packageOrVersionSpec [

	self subclassResponsibility
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> recordingSpecLoader [

	^(MetacelloNullRecordingMCSpecLoader on: self spec)
		shouldDisablePackageCache: self shouldDisablePackageCache;
		loaderPolicy: self loaderPolicy copy;
		yourself
]

{ #category : #repositories }
MetacelloCommonMCSpecLoader >> repositoriesFrom: aMetacelloMVRepositorySpecs [

	^self repositoriesFrom: aMetacelloMVRepositorySpecs ignoreOverrides: false
]

{ #category : #repositories }
MetacelloCommonMCSpecLoader >> repositoriesFrom: aMetacelloMVRepositorySpecs ignoreOverrides: ignoreOverrides [

	| repositories repos |
	(ignoreOverrides not and: [self hasRepositoryOverrides]) ifTrue: [ ^self loaderPolicy overrideRepositories ].
	repositories := MCRepositoryGroup default repositories.
	repos := OrderedCollection new.
	aMetacelloMVRepositorySpecs do: [:aSpec | | description repo |
		description := aSpec description.
		(repo := repositories detect: [:rep | rep description = description ] ifNone: [ aSpec createRepository ]) ~~ nil
			ifTrue: [ repos add: repo ]].
	^repos
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> repositoryMap [

	^self loaderPolicy repositoryMap
]

{ #category : #private }
MetacelloCommonMCSpecLoader >> resolvePackageSpec: packageSpec gofer: gofer [
  | references resolvedReference mcVersion loadedVersionInfos |
  references := self
    retryingResolvePackageSpecReferences: packageSpec
    gofer: gofer.
  resolvedReference := references last asMetacelloCachingResolvedReference.
  mcVersion := resolvedReference version.
  (loadedVersionInfos := self ancestorsFor: packageSpec) ~~ nil
    ifTrue: [ 
      loadedVersionInfos
        do: [ :info | 
          info name = mcVersion info name
            ifTrue: [ 
              | spc |
              "package already loaded, don't load again"
              spc := packageSpec copy.
              spc file: info name.
              (MetacelloIgnorePackageLoaded signal: spc)
                ifFalse: [ ^ nil ] ] ] ].
  ^ resolvedReference
]

{ #category : #private }
MetacelloCommonMCSpecLoader >> resolvePackageSpecReferences: packageSpec gofer: gofer [

	|  versionReference references localGofer |
	localGofer := gofer.
	self hasRepositoryOverrides not
		ifTrue: [
			packageSpec repositorySpecs notEmpty
				ifTrue: [ 
					localGofer := MetacelloGofer new.
					(self repositoriesFrom: packageSpec repositorySpecs) do: [:repo | localGofer repository: repo ]]].
	(packageSpec getFile == nil or: [ self shouldDisablePackageCache ])
		ifTrue: [ 
			"don't use package-cache when trying to get latest version"
			localGofer disablePackageCache ].
	versionReference := packageSpec goferLoaderReference.
	references := versionReference resolveAllWith: localGofer.
	localGofer enablePackageCache.
	^references
]

{ #category : #private }
MetacelloCommonMCSpecLoader >> retryingResolvePackageSpecReferences: packageSpec gofer: gofer [
    | retryCount references repositoryError |
    retryCount := 0.
    references := #().
    [ references isEmpty and: [ retryCount < 3 ] ]
        whileTrue: [ 
            retryCount > 0
                ifTrue: [ 
                    MetacelloNotification signal: '...RETRY->' , packageSpec file ].
            references := [ self resolvePackageSpecReferences: packageSpec gofer: gofer ]
                on: Error , GoferRepositoryError
                do: [ :ex | 
                    self class retryPackageResolution
                        ifFalse: [ ex pass ].
                    retryCount >= 2
                        ifTrue: [ 
                            (ex isKindOf: GoferRepositoryError)
                                ifTrue: [ 
                                    "ignore repository errors at this point, in case an alternate repository is in the list "
                                   MetacelloNotification signal: 'gofer repository error: ' , ex description printString , '...ignoring'.
                                    repositoryError := ex.
                                    ex resume: #() ]
                                ifFalse: [ ex pass ] ].
                    ex return: #() ].
            retryCount := retryCount + 1 ].
    references isEmpty
        ifTrue: [ 
            MetacelloNotification signal: '...FAILED->' , packageSpec file.
            (MetacelloPackageSpecResolutionError new
                packageSpec: packageSpec;
                repositories: gofer repositories;
                repositoryError: repositoryError;
                yourself) signal ].
    ^ references
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> shouldDisablePackageCache [

	disablePackageCache == nil ifTrue: [ disablePackageCache := false ].
	^ disablePackageCache
]

{ #category : #accessing }
MetacelloCommonMCSpecLoader >> shouldDisablePackageCache: anObject [
	disablePackageCache := anObject
]
