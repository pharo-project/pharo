"
MetacelloScriptEngine runs the execution of the script for one projectSpec
"
Class {
	#name : 'MetacelloScriptEngine',
	#superclass : 'Object',
	#instVars : [
		'root',
		'options',
		'ensuredMap',
		'repositoryMap',
		'cacheRepository',
		'specBuilder',
		'loader',
		'projectReferenceSpec'
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

{ #category : 'actions api' }
MetacelloScriptEngine >> do: aBlock required: required [

	^ self
		  do: aBlock
		  required: required
		  onProjectDownGrade: [ :ex :existing :new | ex allowEvenIfLocked ]
		  onProjectUpgrade: [ :ex :existing :new | ex allowEvenIfLocked ]
		  commitIfSuccess: false
]

{ #category : 'actions api' }
MetacelloScriptEngine >> do: aBlock required: required onProjectDownGrade: onDownGradeBlock onProjectUpgrade: onUpgradeBlock commitIfSuccess: aBoolean [

	MetacelloProjectRegistration
		copyRegistryDuring: [
			self handleNotificationsForAction: [
				| versionSpec projectSpec requiredArray |
				self setDefaultsAndValidateProjectSpec.
				[
				projectSpec := (self lookupProjectSpecFor:
					                self projectReferenceSpec projectReference) copy ]
					on: MetacelloAllowProjectDowngrade , MetacelloAllowProjectUpgrade
					do: [ :notification |
						notification
							handleOnDownGrade: onDownGradeBlock
							onUpgrade: onUpgradeBlock ].

				projectSpec loads: required.
				projectSpec ensureProjectLoadedWithEngine: self.
				versionSpec := projectSpec project version: projectSpec versionString.
				requiredArray := projectSpec loadListForVersion: versionSpec.	
				self root: (aBlock value: versionSpec value: requiredArray value: projectSpec) ] ]

		commitIfSuccess: aBoolean
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> doLoadProjectSpecFrom: aMetacelloPackageSpec [

	self loader loadPackageDirective: (MetacelloDirective
			 loadPackage: aMetacelloPackageSpec
			 repositorySpecs: aMetacelloPackageSpec repositorySpecs)
]

{ #category : 'accessing' }
MetacelloScriptEngine >> ensuredMap [

	^ ensuredMap ifNil: [ ensuredMap := Dictionary new ]
]

{ #category : 'actions api' }
MetacelloScriptEngine >> fetch: required [

	^ self
		  do: [ :versionSpec :requiredArray :projectSpec |
			  versionSpec
				  doFetchRequiredFromArray: requiredArray
				  withEngine: self ]
		  required: required
]

{ #category : 'actions api' }
MetacelloScriptEngine >> get [
	" load a fresh copy from repo"

	MetacelloProjectRegistration
		copyRegistryDuring: [
			self handleNotificationsForAction: [
				| spec projectPackage |
				self setDefaultsAndValidateProjectSpec.
				spec := self projectReferenceSpec.
				projectPackage := spec projectPackage.
				projectPackage repositorySpecs do: [ :repoSpec |
					repoSpec createRepository flushForScriptGet ].
				self doLoadProjectSpecFrom: projectPackage.
				self root: (Smalltalk at: spec className asSymbol) project.
				MetacelloProjectRegistration
					registrationForProjectSpec: spec
					ifAbsent: [ :new | new registerProject ]
					ifPresent: [ :existing :new |
						existing copyOnWrite: [ :existingCopy |
							spec copyForRegistration: existingCopy onWrite: [ :specCopy |
								specCopy
									ifNil: [ existingCopy merge: new ]
									ifNotNil: [ specCopy mergeScriptRepository: spec ] ] ] ] ] ]
		commitIfSuccess: true
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

{ #category : 'initialization' }
MetacelloScriptEngine >> initialize [

	super initialize.
	specBuilder := MetacelloSpecBuilder new
]

{ #category : 'actions api' }
MetacelloScriptEngine >> list [
  self setDefaultsAndValidateProjectSpec.
  self root: self projectReferenceSpec
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

	self
		do: [ :versionSpec :requiredArray :projectSpec |
			| result |
			result := versionSpec
				          doLoadRequiredFromArray: requiredArray
				          withEngine: self.
			self root: result.
			MetacelloProjectRegistration
				registrationForProjectSpec: projectSpec
				ifAbsent: [ :new |
					new
						loadedInImage: true;
						registerProject ]
				ifPresent: [ :existing :new |
					existing copyOnWrite: [ :existingCopy |
						existingCopy
							loadedInImage: true;
							merge: new ] ] ]
		required: required
		onProjectDownGrade: onDownGradeBlock
		onProjectUpgrade: onUpgradeBlock
		commitIfSuccess: true
]

{ #category : 'accessing' }
MetacelloScriptEngine >> loader [

	^ (self options
		   at: #loader
		   ifAbsentPut: [ MetacelloMonticelloLoader new ])
		  engine: self;
		  yourself
]

{ #category : 'actions api' }
MetacelloScriptEngine >> lock [

	| spec |
	MetacelloProjectRegistration
		copyRegistryDuring: [
			self setDefaultsAndValidate:
				self projectReferenceSpec projectReference copy. "don't add defaults"
			spec := self projectReferenceSpec projectReference.
			MetacelloProjectRegistration
				registrationForProjectSpec: spec
				ifAbsent: [ :new |
					new
						locked: true;
						registerProject ]
				ifPresent: [ :existing :new |
					existing copyOnWrite: [ :existingCopy |
						existingCopy locked: true.
						spec copyForRegistration: existingCopy onWrite: [ :specCopy |
							specCopy ifNil: [ existingCopy merge: new ] ifNotNil: [
								specCopy mergeScriptRepository: spec.
								spec := specCopy ] ] ] ].
			self root: spec ]
		commitIfSuccess: true
]

{ #category : 'accessing' }
MetacelloScriptEngine >> lookupProjectClassNamed: aString [ 
	
	^ self loader lookupProjectClassNamed: aString 
]

{ #category : 'project lookup' }
MetacelloScriptEngine >> lookupProjectSpecFor: aProjectSpec [
	"if there is no conflict, choose new spec"

	| registration |
	registration := MetacelloProjectRegistration
		                registrationForProjectSpec: aProjectSpec
		                ifAbsent: [ :new | new ]
		                ifPresent: [ :existing :new |
		                self resolvePresentProject: existing new: new ].

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
MetacelloScriptEngine >> projectReferenceSpec [
    ^ projectReferenceSpec
]

{ #category : 'accessing' }
MetacelloScriptEngine >> projectReferenceSpec: aProjectSpec [

    projectReferenceSpec := aProjectSpec
]

{ #category : 'actions api' }
MetacelloScriptEngine >> record: required [

	^ self
		  do: [ :versionSpec :requiredArray |
			  versionSpec
				  doRecordRequiredFromArray: requiredArray
				  withEngine: self ]
		  required: required
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
  issues := aProjectSpec projectReference
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

  self setDefaultsAndValidate: self projectReferenceSpec
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
		copyRegistryDuring: [
			self setDefaultsAndValidate:
				self projectReferenceSpec projectReference copy. "don't add defaults"
			spec := self projectReferenceSpec projectReference.
			MetacelloProjectRegistration
				registrationForProjectSpec: spec
				ifAbsent: [ :ignored |  ]
				ifPresent: [ :existing :new |
					existing copyOnWrite: [ :existingCopy |
						existingCopy locked: false ] ].
			self root: spec ]
		commitIfSuccess: true
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
