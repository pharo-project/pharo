Class {
	#name : 'MetacelloTarget',
	#superclass : 'Object',
	#instVars : [
		'operator',
		'loaderPolicy',
		'spec'
	],
	#classVars : [
		'RetryPackageResolution'
	],
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'class initialization' }
MetacelloTarget class >> initialize [

	self deprecatedAliases: { #MetacelloCommonMCSpecLoader }
]

{ #category : 'utilities' }
MetacelloTarget class >> nameComponentsFrom: aVersionName [
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

{ #category : 'instance creation' }
MetacelloTarget class >> on: aMetacelloPackagesSpec [

	^ self new
		  spec: aMetacelloPackagesSpec;
		  yourself
]

{ #category : 'accessing' }
MetacelloTarget class >> retryPackageResolution [
	"if true, 
		errors during #retryingResolvePackageSpecReferences:gofer: are caught and 
		the resolution is retried 3 times. After the thrid time, a MetacelloPackageSpecResolutionError
		is thrown
	if false,
		an error during #retryingResolvePackageSpecReferences:gofer: will be passed,
		likely resulting in a walkback ... useful for debugging."

	^ RetryPackageResolution ifNil: [ RetryPackageResolution := true ]
]

{ #category : 'accessing' }
MetacelloTarget class >> retryPackageResolution: aBool [
    RetryPackageResolution := aBool
]

{ #category : 'versionInfo' }
MetacelloTarget >> ancestorsFor: packageSpec [

	^ packageSpec ancestors
]

{ #category : 'accessing' }
MetacelloTarget >> asEnsureTarget [
	^ self
]

{ #category : 'accessing' }
MetacelloTarget >> asFetchingTarget [

	^self
]

{ #category : 'accessing' }
MetacelloTarget >> asLoadingTarget [

	^self
]

{ #category : 'accessing' }
MetacelloTarget >> asRecordingTarget [

	^(MetacelloRecordTarget on: self spec)
		loaderPolicy: self loaderPolicy copy;
		yourself
]

{ #category : 'versionInfo' }
MetacelloTarget >> currentVersionInfoFor: packageSpec [

	^ packageSpec currentVersionInfo
]

{ #category : 'actions' }
MetacelloTarget >> doLoad [
	"NOOP"

	
]

{ #category : 'actions' }
MetacelloTarget >> doingLoads: aBlock [
	"escape mechanism for recording and null loaders to skip doing loaderlike things"
	
	aBlock value
]

{ #category : 'testing' }
MetacelloTarget >> ensureForDevelopment [
  ^ true
]

{ #category : 'testing' }
MetacelloTarget >> hasRepositoryOverrides [

	^self loaderPolicy hasRepositoryOverrides
]

{ #category : 'accessing' }
MetacelloTarget >> ignoreImage [

	^self loaderPolicy ignoreImage
]

{ #category : 'initialization' }
MetacelloTarget >> initialize [

	self loaderPolicy
]

{ #category : 'actions' }
MetacelloTarget >> linearLoadPackageSpec: packageSpec gofer: gofer [

	self subclassResponsibility
]

{ #category : 'private' }
MetacelloTarget >> linearLoadPackagesOfSpec: aSpec [
	
	| gofer monticelloRepositories |
	gofer := Gofer new.
	monticelloRepositories := self repositoriesFrom: aSpec repositorySpecs.
	monticelloRepositories do: [:repo | gofer repository: repo ].
	aSpec packageSpecsInLoadOrder do: [:pkg | pkg loadUsing: self gofer: gofer ].
]

{ #category : 'actions' }
MetacelloTarget >> load [

	self loadType == #atomic ifTrue: [
		^ self atomicLoadPackageOfSpec: self spec ].
	self loadType == #linear ifTrue: [
		^ self linearLoadPackagesOfSpec: self spec ].

	MetacelloError signal:
		'Unknown loadType: ' , self loadType printString
]

{ #category : 'actions' }
MetacelloTarget >> loadPackageDirectives: pkgLoads gofer: aGofer [

	MetacelloPlatform current
		do: [
			| goferLoad loadBlock answers |
			goferLoad := MetacelloGoferLoad on: aGofer.
			answers := OrderedCollection new.

			pkgLoads do: [ :packageLoadDirective |
				| packageSpec |
				packageSpec := packageLoadDirective spec.
				aGofer disablePackageCache. "for good luck:)"
				(self resolvePackageSpec: packageSpec gofer: aGofer) ifNotNil: [
					:resolvedReference |
					goferLoad addResolved: resolvedReference.
					answers addAll: packageSpec answers.
					packageLoadDirective resolvedReference: resolvedReference ] ].


			loadBlock := [
			             goferLoad execute.
			             pkgLoads do: [ :packageLoadDirective |
				             packageLoadDirective resolvedReference
					             ifNotNil: [
						             MetacelloNotification
							             signal:
								             'Loaded -> ' , packageLoadDirective file
								             , ' --- '
								             ,
								             packageLoadDirective repository
									             repositoryDescription , ' --- '
								             ,
								             packageLoadDirective resolvedReference repository
									             description
							             level: 2.
						             packageLoadDirective resolvedReference workingCopy
							             repositoryGroup addRepository:
							             packageLoadDirective repository ]
					             ifNil: [
						             MetacelloNotification
							             signal:
							             'Already Loaded -> ' , packageLoadDirective file
							             level: 2 ] ] ].
			answers notEmpty
				ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
				ifFalse: [ loadBlock value ] ]
		displaying: 'Loading...'
]

{ #category : 'accessing' }
MetacelloTarget >> loadType [
	"#atomic or #linear"
	
	^self project loadType
]

{ #category : 'accessing' }
MetacelloTarget >> loaderPolicy [

	^ loaderPolicy ifNil: [ loaderPolicy := MetacelloLoaderPolicy new ]
]

{ #category : 'accessing' }
MetacelloTarget >> loaderPolicy: anObject [
	loaderPolicy := anObject
]

{ #category : 'packages' }
MetacelloTarget >> nameComponentsFrom: aVersionName [

	^self class nameComponentsFrom: aVersionName
]

{ #category : 'accessing' }
MetacelloTarget >> operator [

	^ operator ifNil: [ #= ]
]

{ #category : 'accessing' }
MetacelloTarget >> operator: aSymbol [

	operator := aSymbol
]

{ #category : 'doits' }
MetacelloTarget >> postLoad: packageOrVersionSpec [
	"subclassResponsibility, but it gets called during an upgrade, so leave it as NOOP"
]

{ #category : 'doits' }
MetacelloTarget >> preLoad: packageOrVersionSpec [

	self subclassResponsibility
]

{ #category : 'accessing' }
MetacelloTarget >> project [

	^self spec project
]

{ #category : 'repositories' }
MetacelloTarget >> repositoriesFrom: aMetacelloMVRepositorySpecs [

	^self repositoriesFrom: aMetacelloMVRepositorySpecs ignoreOverrides: false
]

{ #category : 'repositories' }
MetacelloTarget >> repositoriesFrom: aMetacelloMVRepositorySpecs ignoreOverrides: ignoreOverrides [

	| repositories repos |
	(ignoreOverrides not and: [ self hasRepositoryOverrides ]) ifTrue: [ ^ self loaderPolicy overrideRepositories ].
	repositories := MCRepositoryGroup default repositories.
	repos := OrderedCollection new.
	aMetacelloMVRepositorySpecs do: [ :aSpec |
		| description |
		description := aSpec description.
		(repositories
			 detect: [ :rep | rep description = description ]
			 ifNone: [ aSpec createRepository ]) ifNotNil: [ :repo | repos add: repo ] ].
	^ repos
]

{ #category : 'accessing' }
MetacelloTarget >> repositoryMap [

	^self loaderPolicy repositoryMap
]

{ #category : 'private' }
MetacelloTarget >> resolvePackageSpec: packageSpec gofer: gofer [

	| references resolvedReference mcVersion |
	references := self retryingResolvePackageSpecReferences: packageSpec gofer: gofer.
	resolvedReference := references last asMetacelloCachingResolvedReference.
	mcVersion := resolvedReference version.
	(self ancestorsFor: packageSpec) ifNotNil: [ :loadedVersionInfos |
		loadedVersionInfos do: [ :info |
			info name = mcVersion info name ifTrue: [
				| spc |
				"package already loaded, don't load again"
				spc := packageSpec copy.
				spc file: info name.
				(MetacelloIgnorePackageLoaded signal: spc) ifFalse: [ ^ nil ] ] ] ].
	^ resolvedReference
]

{ #category : 'private' }
MetacelloTarget >> resolvePackageSpecReferences: packageSpec gofer: gofer [

	| versionReference references localGofer |
	localGofer := gofer.
"	1halt."
	self hasRepositoryOverrides not ifTrue: [
"		1halt."
		packageSpec repositorySpecs notEmpty ifTrue: [
			localGofer := Gofer new.
			(self repositoriesFrom: packageSpec repositorySpecs) do: [ :repo |
				localGofer repository: repo ] ] ].
	packageSpec getFile ifNil: [ "don't use package-cache when trying to get latest version"
		localGofer disablePackageCache ].
	versionReference := packageSpec goferLoaderReference.
	references := versionReference resolveAllWith: localGofer.
	localGofer enablePackageCache.
	^ references
]

{ #category : 'private' }
MetacelloTarget >> retryingResolvePackageSpecReferences: packageSpec gofer: gofer [
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

{ #category : 'accessing' }
MetacelloTarget >> spec [

	^spec
]

{ #category : 'accessing' }
MetacelloTarget >> spec: aMetacelloPackagesSpec [

	spec := aMetacelloPackagesSpec
]
