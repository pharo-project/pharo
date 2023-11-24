Class {
	#name : 'MetacelloTarget',
	#superclass : 'Object',
	#instVars : [
		'operator'
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

{ #category : 'actions' }
MetacelloTarget >> doLoad [
	"NOOP"

	
]

{ #category : 'actions' }
MetacelloTarget >> doingLoads: aBlock [
	"escape mechanism for recording and null loaders to skip doing loaderlike things"
	
	aBlock value
]

{ #category : 'initialization' }
MetacelloTarget >> initialize [

	super initialize.
	"self loaderPolicy"
]

{ #category : 'actions' }
MetacelloTarget >> linearLoadPackageSpec: packageSpec gofer: gofer withEngine: anEngine [

	self subclassResponsibility
]

{ #category : 'private' }
MetacelloTarget >> linearLoadPackagesOfSpec: aSpec withEngine: anEngine [

	| gofer monticelloRepositories |
	gofer := Gofer new.
	monticelloRepositories := self repositoriesFrom:
		                          aSpec repositorySpecs.
	monticelloRepositories do: [ :repo | gofer repository: repo ].
	aSpec packageSpecsInLoadOrder do: [ :pkg |
		pkg loadUsing: self gofer: gofer withEngine: anEngine ]
]

{ #category : 'actions' }
MetacelloTarget >> load: aSpec withEngine: anEngine [

	aSpec project loadType == #atomic ifTrue: [
		^ self atomicLoadPackageOfSpec: aSpec withEngine: anEngine ].
	aSpec project loadType == #linear ifTrue: [
		^ self linearLoadPackagesOfSpec: aSpec withEngine: anEngine ].

	MetacelloError signal:
		'Unknown loadType: ' , aSpec project loadType printString
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
MetacelloTarget >> postLoad: packageOrVersionSpec withEngine: anEngine [
	"subclassResponsibility, but it gets called during an upgrade, so leave it as NOOP"
]

{ #category : 'doits' }
MetacelloTarget >> preLoad: packageOrVersionSpec withEngine: anEngine [

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
"	(ignoreOverrides not and: [ self hasRepositoryOverrides ]) ifTrue: [ ^ self loaderPolicy overrideRepositories ]."
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

{ #category : 'private' }
MetacelloTarget >> resolvePackageSpec: packageSpec gofer: gofer [

	| references resolvedReference mcVersion |
	references := self
		              retryingResolvePackageSpecReferences: packageSpec
		              gofer: gofer.
	resolvedReference := references last
		                     asMetacelloCachingResolvedReference.
	mcVersion := resolvedReference version.
	packageSpec ancestors ifNotNil: [ :loadedVersionInfos |
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
	"self hasRepositoryOverrides not ifTrue: ["
"		1halt."
		packageSpec repositorySpecs notEmpty ifTrue: [
			localGofer := Gofer new.
			(self repositoriesFrom: packageSpec repositorySpecs) do: [ :repo |
				localGofer repository: repo ] ] "]".
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
