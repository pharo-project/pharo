"
MetacelloScriptEngine runs the execution of the script for one projectSpec
"
Class {
	#name : 'MetacelloScriptEngine',
	#superclass : 'Object',
	#instVars : [
		'root',
		'projectSpec',
		'options',
		'ensuredMap',
		'repositoryMap',
		'cacheGofer',
		'cacheRepository',
		'overrideRepositories',
		'specBuilder',
		'loader'
	],
	#classVars : [
		'DefaultRepositoryDescription',
		'DefaultVersionString'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'utilities' }
MetacelloScriptEngine class >> baseNameOf: className [
  ^ (className beginsWith: 'BaselineOf')
    ifTrue: [ className copyFrom: 'BaselineOf' size + 1 to: className size ]
    ifFalse: [ 
      (className beginsWith: 'ConfigurationOf')
        ifTrue: [ className copyFrom: 'ConfigurationOf' size + 1 to: className size ]
        ifFalse: [ className ] ]
]

{ #category : 'utilities' }
MetacelloScriptEngine class >> baselineNameFrom: baseName [
    "Return the fully-qualified configuration class name."

    ^ (baseName indexOfSubCollection: 'BaselineOf') > 0
        ifTrue: [ baseName ]
        ifFalse: [ 'BaselineOf' , baseName ]
]

{ #category : 'utilities' }
MetacelloScriptEngine class >> configurationNameFrom: baseName [
	"Return the fully-qualified configuration class name."

	^ ((baseName beginsWith: 'ConfigurationOf')
		   ifTrue: [ baseName ]
		   ifFalse: [ 'ConfigurationOf' , baseName ]) asSymbol
]

{ #category : 'defaults' }
MetacelloScriptEngine class >> defaultRepositoryDescription [

	DefaultRepositoryDescription ifNil: [ DefaultRepositoryDescription := 'http://smalltalkhub.com/mc/Pharo/MetaRepoForPharo30/main' ].
	^ DefaultRepositoryDescription
]

{ #category : 'defaults' }
MetacelloScriptEngine class >> defaultRepositoryDescription: descriptionOrNil [
  DefaultRepositoryDescription := descriptionOrNil
]

{ #category : 'defaults' }
MetacelloScriptEngine class >> defaultVersionString [
    DefaultVersionString ifNil: [ DefaultVersionString := #'stable' ].
    ^ DefaultVersionString
]

{ #category : 'accessing' }
MetacelloScriptEngine >> cacheGofer [

	^ cacheGofer ifNil: [ "don't use a caching Gofer here, since we expect the contents to change during a fetch operation"
		  cacheGofer := Gofer new
			                disablePackageCache;
			                repository: self cacheRepository;
			                yourself ]
]

{ #category : 'accessing' }
MetacelloScriptEngine >> cacheRepository [

	^ cacheRepository ifNil: [ cacheRepository := MCDictionaryRepository new ]
]

{ #category : 'defaults' }
MetacelloScriptEngine >> defaultRepositoryDescription [
    ^ self class defaultRepositoryDescription
]

{ #category : 'defaults' }
MetacelloScriptEngine >> defaultVersionString [
    ^ self class defaultVersionString
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> doLoadProjectSpecFrom: aMetacelloPackageSpec [

	"| gofer monticelloRepositories externalReference |"
"	gofer := Gofer new.
	monticelloRepositories := self loader repositoriesFrom:
		                          aMetacelloPackageSpec repositorySpecs.
	monticelloRepositories do: [ :repo | gofer repository: repo ].


	externalReference := self loader
		                     resolveRecordingPackageSpecReference:
		                     aMetacelloPackageSpec
		                     gofer: gofer."

	self loader loadPackageDirective: (MetacelloDirective
			 loadPackage: aMetacelloPackageSpec
			 externalReference: aMetacelloPackageSpec repositorySpecs)
]

{ #category : 'accessing' }
MetacelloScriptEngine >> ensuredMap [

	^ ensuredMap ifNil: [ ensuredMap := Dictionary new ]
]

{ #category : 'actions api' }
MetacelloScriptEngine >> fetch: required [

	MetacelloProjectRegistration copyRegistryWhile: [
		self handleNotificationsForAction: [
			| version1 loadedSpec |
			self setDefaultsAndValidateProjectSpec.
			[ loadedSpec := self lookupProjectSpecFor: self projectSpec ]
				on: MetacelloAllowProjectDowngrade , MetacelloAllowProjectUpgrade
				do: [ :notification |
					notification
						handleOnDownGrade: [ :ex :existing :new | ex allowEvenIfLocked ]
						onUpgrade: [ :ex :existing :new | ex allowEvenIfLocked ] ].
			version1 := loadedSpec versionForScriptEngine: self.
			self root: ([ :version |
				 required isEmpty
					 ifTrue: [ version fetchWithEngine: self ]
					 ifFalse: [ version fetch: required ] ] value: version1) ] ]
]

{ #category : 'actions api' }
MetacelloScriptEngine >> get [
  " load a fresh copy from repo"

  | spec projectPackage |
  MetacelloProjectRegistration
    copyRegistryRestoreOnErrorWhile: [ 
      self setDefaultsAndValidateProjectSpec.
      spec := self projectSpec.
      projectPackage := spec projectPackage.
      projectPackage repositorySpecs
        do: [ :repoSpec | repoSpec createRepository flushForScriptGet ].
      projectPackage load.
      self root: (Smalltalk at: spec className asSymbol) project.
      MetacelloProjectRegistration
        registrationForProjectSpec: spec
        ifAbsent: [ :new | new registerProject ]
        ifPresent: [ :existing :new | 
          existing
            copyOnWrite: [ :existingCopy | 
              spec
                copyForRegistration: existingCopy
                onWrite: [ :specCopy | 
                  specCopy
                    ifNil: [ existingCopy merge: new ]
                    ifNotNil: [ specCopy mergeScriptRepository: spec ] ] ] ] ]
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> getBaselineProjectUnconditionalLoad [

	| project |
	[
	self doLoadProjectSpecFrom: (MetacelloPackageSpec new
			 name: self projectSpec className;
			 repositories: self projectSpec repositories;
			 yourself) ]
		on: MetacelloIgnorePackageLoaded
		do: [ :ex |
      "see https://github.com/dalehenrich/metacello-work/issues/244 ... uncoditionally load baseline"
			ex resume: true ].
	project := (Smalltalk at: self projectSpec className asSymbol) new
      initalizeProjectWithRepositoryDescription: self projectSpec repositories map keys.
  project version spec repositories: self repositories copy.
  ^ project
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> getConfigurationProjectUnconditionalLoad: unconditionalLoad [
    ^ (self getConfigurationUnconditionalLoad: unconditionalLoad) project
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> getConfigurationUnconditionalLoad: unconditionalLoad [

	| spec |
	spec := self projectSpec.
	Smalltalk at: spec className asSymbol ifPresent: [ :cl | ^ cl ].
	(spec := self lookupProjectSpecFor: spec) projectPackage load.
	^ Smalltalk at: spec className asSymbol
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleConflict: exception [
    ^ (self options at: #'onConflict' ifAbsent: [ ^ exception pass ])
        cull: exception
        cull: exception existingProjectRegistration
        cull: exception newProjectRegistration
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleDowngrade: exception [
    ^ (self options at: #'onDowngrade' ifAbsent: [ ^ exception pass ])
        cull: exception
        cull: exception existingProjectRegistration
        cull: exception newProjectRegistration
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleLock: exception [
  ^ (self options at: #'onLock' ifAbsent: [ ^ exception pass ])
    cull: exception
    cull: exception existingProjectRegistration
    cull: exception newProjectRegistration
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleLookupBaselineSpecForEnsureLoad: exception [
  "if requested and registered don't compare equal, then ensure the new baseline is loaded"

  | requested registered |
  requested := exception projectSpec.
  registered := self lookupProjectSpecFor: exception projectSpec.
  ^ exception resume: (registered compareEqual: requested) not
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleLookupProjectSpec: aProjectSpec [

	^ (self lookupProjectSpecFor: aProjectSpec) ifNil: [ ^ aProjectSpec ]
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleLookupProjectSpecForLoad: aProjectSpec [
	"if overrideProjectSpec is nil, use currentVersion in image, ignoreImage is false"

	| override registered |
	registered := self lookupProjectSpecFor: aProjectSpec.
	(registered compareEqual: aProjectSpec) ifFalse: [ "counts as override, only if they differ in some aspect"
		override := registered ].
	^ MetacelloProjectSpecForLoad new
		  projectSpec: aProjectSpec;
		  overrideProjectSpec: override;
		  yourself
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleNotificationsForAction: actionBlock [

	[
	[
	actionBlock
		on: MetacelloAllowProjectDowngrade , MetacelloAllowProjectUpgrade
			, MetacelloAllowConflictingProjectUpgrade
		do: [ :ex | "option handlers need to be outermost set of handlers ... last line of defense before users are involved"
			ex handleResolutionFor: self ] ]
		on: MetacelloAllowLockedProjectChange
		do: [ :ex | "MetacelloAllowLockedProjectChange need to be outermost handler ... since it is signaled from second line of handlers"
			ex handleResolutionFor: self ] ]
		on: Warning
		do: [ :ex | "Warning is absolute outermost handler"
			self handleWarning: ex ]
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleProjectSpecLoaded: aProjectSpec [

	MetacelloProjectRegistration
		registrationForProjectSpec: aProjectSpec
		ifAbsent: [ :new |
			new
				loadedInImage: true;
				registerProject ]
		ifPresent: [ :existing :new | "unconditionally merge new with existing (updates registration)"
			existing copyOnWrite: [ :existingCopy |
				existingCopy
					loadedInImage: true;
					merge: new ] ]
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleUpgrade: exception [
    ^ (self options at: #'onUpgrade' ifAbsent: [ ^ exception pass ])
        cull: exception
        cull: exception existingProjectRegistration
        cull: exception newProjectRegistration
]

{ #category : 'handlers' }
MetacelloScriptEngine >> handleWarning: exception [
  ^ (self options at: #'onWarning' ifAbsent: [ ^ exception pass ])
    cull: exception
]

{ #category : 'options' }
MetacelloScriptEngine >> ignoreImage [
    ^ self options at: #'ignoreImage' ifAbsent: [ false ]
]

{ #category : 'options' }
MetacelloScriptEngine >> initialize [

	super initialize.
	specBuilder := MetacelloSpecBuilder new
]

{ #category : 'actions api' }
MetacelloScriptEngine >> list [
  self setDefaultsAndValidateProjectSpec.
  self root: self projectSpec
]

{ #category : 'actions api' }
MetacelloScriptEngine >> load: required [
    self
        load: required
        onProjectDownGrade: [ :ex :existing :new | ex allowEvenIfLocked ]
        onProjectUpgrade: [ :ex :existing :new | ex allowEvenIfLocked ]
]

{ #category : 'actions api' }
MetacelloScriptEngine >> load: required onProjectDownGrade: onDownGradeBlock onProjectUpgrade: onUpgradeBlock [

	| result |
	MetacelloProjectRegistration copyRegistryRestoreOnErrorWhile: [
		self handleNotificationsForAction: [
			| version loadedSpec requiredArray |
			self setDefaultsAndValidateProjectSpec.
			[ loadedSpec := (self lookupProjectSpecFor: self projectSpec) copy ]
				on: MetacelloAllowProjectDowngrade , MetacelloAllowProjectUpgrade
				do: [ :ex |
					ex handleOnDownGrade: onDownGradeBlock onUpgrade: onUpgradeBlock ].
			version := loadedSpec versionForScriptEngine: self.
			requiredArray := required isString
				                 ifTrue: [ { required } ]
				                 ifFalse: [ required ].
			requiredArray ifEmpty: [
				requiredArray := version spec defaultPackageNames ].
			result := version
				          doLoadRequiredFromArray: requiredArray
				          withEngine: self.
			self root: result.
			loadedSpec loads: required.
			MetacelloProjectRegistration
				registrationForProjectSpec: loadedSpec
				ifAbsent: [ :new |
					new
						loadedInImage: true;
						registerProject ]
				ifPresent: [ :existing :new |
					existing copyOnWrite: [ :existingCopy |
						existingCopy
							loadedInImage: true;
							merge: new ] ] ] ]
]

{ #category : 'accessing' }
MetacelloScriptEngine >> loader [

	^ loader ifNil: [ loader := MetacelloGoferLoader new ]
]

{ #category : 'actions api' }
MetacelloScriptEngine >> lock [
  | spec |
  MetacelloProjectRegistration
    copyRegistryRestoreOnErrorWhile: [ 
      self setDefaultsAndValidate: self projectSpec copy.	"don't add defaults"
      spec := self projectSpec.
      MetacelloProjectRegistration
        registrationForProjectSpec: spec
        ifAbsent: [ :new | 
          new
            locked: true;
            registerProject ]
        ifPresent: [ :existing :new | 
          existing
            copyOnWrite: [ :existingCopy | 
              existingCopy locked: true.
              spec
                copyForRegistration: existingCopy
                onWrite: [ :specCopy | 
                  specCopy
                    ifNil: [ existingCopy merge: new ]
                    ifNotNil: [ 
                      specCopy mergeScriptRepository: spec.
                      spec := specCopy ] ] ] ].
      self root: spec ]
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> lookupProjectSpecFor: aProjectSpec [
	"if there is no conflict, choose new spec"
	| registration |
	
	registration := MetacelloProjectRegistration
		registrationForProjectSpec: aProjectSpec
		ifAbsent: [ :new | new ]
		ifPresent: [ :existing :new | self resolvePresentProject: existing new: new ].
  
	^ registration projectSpec
]

{ #category : 'accessing' }
MetacelloScriptEngine >> options [
    options ifNil: [ options := Dictionary new ].
    ^ options
]

{ #category : 'accessing' }
MetacelloScriptEngine >> options: aDictionary [
    options := aDictionary
]

{ #category : 'accessing' }
MetacelloScriptEngine >> projectName [
    ^ self projectSpec name
]

{ #category : 'accessing' }
MetacelloScriptEngine >> projectSpec [
    ^ projectSpec
]

{ #category : 'accessing' }
MetacelloScriptEngine >> projectSpec: aProjectSpec [

    projectSpec := aProjectSpec
]

{ #category : 'actions api' }
MetacelloScriptEngine >> record: required [

	MetacelloProjectRegistration copyRegistryWhile: [
		self handleNotificationsForAction: [
			| version loadedSpec requiredArray |
			self setDefaultsAndValidateProjectSpec.
			[ loadedSpec := self lookupProjectSpecFor: self projectSpec ]
				on: MetacelloAllowProjectDowngrade , MetacelloAllowProjectUpgrade
				do: [ :notification |
					notification
						handleOnDownGrade: [ :ex :existing :new | ex allowEvenIfLocked ]
						onUpgrade: [ :ex :existing :new | ex allowEvenIfLocked ] ].
			version := loadedSpec versionForScriptEngine: self.

			requiredArray := required isString
				ifTrue: [ { required } ]
				ifFalse: [ required ].
			requiredArray ifEmpty: [
				requiredArray := version spec defaultPackageNames ].
			self root:
				(version doRecordRequiredFromArray: requiredArray withEngine: self) ] ].
	^ self
]

{ #category : 'actions api' }
MetacelloScriptEngine >> register [
  | spec |
  self setDefaultsAndValidate: self projectSpec copy.	"don't add defaults"
  spec := self projectSpec.
  MetacelloProjectRegistration
    registrationForProjectSpec: spec
    ifAbsent: [ :new | new registerProject ]
    ifPresent: [ :existing :new | existing copyOnWrite: [ :existingCopy | existingCopy merge: new ] ].
  self root: spec
]

{ #category : 'accessing' }
MetacelloScriptEngine >> repositories [
    ^ self projectSpec repositories
]

{ #category : 'accessing' }
MetacelloScriptEngine >> repositoryMap [

	^ repositoryMap ifNil: [ repositoryMap := Dictionary new ]
]

{ #category : 'initialization' }
MetacelloScriptEngine >> resetCacheGofer [

	cacheGofer := nil
]

{ #category : 'private' }
MetacelloScriptEngine >> resolvePresentProject: existingRegistration new: newRegistration [

	existingRegistration locked 
		ifTrue: [ ^ existingRegistration ].

	^ (existingRegistration hasLoadConflicts: newRegistration)
		ifTrue: [
			((existingRegistration canUpgradeTo: newRegistration)
				ifTrue: [ MetacelloAllowProjectUpgrade new ]
				ifFalse: [
					(existingRegistration canDowngradeTo: newRegistration)
						ifTrue: [ MetacelloAllowProjectDowngrade new ]
						ifFalse: [ MetacelloAllowConflictingProjectUpgrade new ] ])
			existingProjectRegistration: existingRegistration;
			newProjectRegistration: newRegistration;
			signal ]
		ifFalse: [ newRegistration ]
]

{ #category : 'accessing' }
MetacelloScriptEngine >> root [
	^ root
]

{ #category : 'accessing' }
MetacelloScriptEngine >> root: anObject [
	root := anObject
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> setDefaultsAndValidate: aProjectSpec [
  "NOTE: aProjectSpec has defaults assigned if versionString or repository missing"

  | issues |
  issues := aProjectSpec
    validateForScriptLoad: self
    withDefaultVersionString: self defaultVersionString
    withDefaultRepositoryDecription: self defaultRepositoryDescription.
  issues isEmpty
    ifTrue: [ ^ self ].
  (MetacelloValidationFailure
    issues: issues
    message: 'Project spec validation failure') signal
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> setDefaultsAndValidateProjectSpec [
  "NOTE: projectSpec has defaults assigned if versionString or repository missing"

  self setDefaultsAndValidate: self projectSpec
]

{ #category : 'options' }
MetacelloScriptEngine >> silently [
    ^ self options at: #'silently' ifAbsent: [ false ]
]

{ #category : 'accessing' }
MetacelloScriptEngine >> specBuilder [
	^ specBuilder
]

{ #category : 'actions api' }
MetacelloScriptEngine >> unlock [
  | spec |
  MetacelloProjectRegistration
    copyRegistryRestoreOnErrorWhile: [ 
      self setDefaultsAndValidate: self projectSpec copy.	"don't add defaults"
      spec := self projectSpec.
      MetacelloProjectRegistration
        registrationForProjectSpec: spec
        ifAbsent: [ :ignored |  ]
        ifPresent: [ :existing :new | existing copyOnWrite: [ :existingCopy | existingCopy locked: false ] ].
      self root: spec ]
]

{ #category : 'actions api' }
MetacelloScriptEngine >> unregister [
  | spec |
  self setDefaultsAndValidate: self projectSpec copy.	"don't add defaults"
  spec := self projectSpec.
  MetacelloProjectRegistration
    registrationForProjectSpec: spec
    ifAbsent: [ :ignored |  ]
    ifPresent: [ :existing :new | existing unregisterProject ].
  self root: spec
]
