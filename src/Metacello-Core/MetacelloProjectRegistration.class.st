Class {
	#name : #MetacelloProjectRegistration,
	#superclass : #Object,
	#instVars : [
		'projectName',
		'configurationProjectSpec',
		'baselineProjectSpec',
		'loadedInImage',
		'locked',
		'mutable',
		'versionInfo'
	],
	#classVars : [
		'Registry'
	],
	#category : #'Metacello-Core-Scripts'
}

{ #category : #accessing }
MetacelloProjectRegistration class >> baselineClasses [
    "Return a set of the Metacello baseline classes that have been loaded into the image."

    "self baselineClasses"

    ^ BaselineOf allSubclasses
]

{ #category : #accessing }
MetacelloProjectRegistration class >> baselineProjectSpecs [
    "MetacelloProjectRegistration baselineProjectSpecs"

    ^ self registry baselineProjectSpecs
]

{ #category : #accessing }
MetacelloProjectRegistration class >> classRemoved: aClassRemovalAnnouncement [
  "aRemovalAnnouncement is platform-specific ... responds to #itemClass to 
   answer the class that was removed."

  | aClass registration |
  aClass := aClassRemovalAnnouncement itemClass.
  registration := self registry
    registrationForExactClassNamed: aClass name asString
    ifAbsent: [ ^ self ].
  registration unregisterProject
]

{ #category : #accessing }
MetacelloProjectRegistration class >> configurationClasses [
	"Return a set of the Metacello configuration classes that have been loaded into the image."

	"self configurationClasses"

	| answer |
	answer := IdentitySet new.
	ConfigurationOf allSubclasses
		do: [ :cl | 
			(cl includesBehavior: BaselineOf)
				ifFalse: [ answer add: cl ] ].
	Object allSubclasses
		do: [ :cl | 
			(answer includes: cl)
				ifFalse: [ (([ cl isMetacelloConfig ]
						on: Error
						do: [ :ex | ex return: false ])
						and: [ cl name asString beginsWith: 'ConfigurationOf' ])
						ifTrue: [ answer add: cl ] ] ].
	^ answer
]

{ #category : #accessing }
MetacelloProjectRegistration class >> configurationProjectSpecs [
    "MetacelloProjectRegistration configurationProjectSpecs"

    ^ self registry configurationProjectSpecs
]

{ #category : #mutability }
MetacelloProjectRegistration class >> copyRegistryRestoreOnErrorWhile: aBlock [
  "install copy of registry for duration of <aBlock> execution."

  "registrations will be copied on write during <aBlock> execution."

  "if <aBlock> does not return control to this context, revert to the original
	version of the registry. Otherwise leave the new copy installed."

  | oldRegistry newRegistry |
  oldRegistry := self registry.
  newRegistry := self registry copy.
  self registry: newRegistry.
  aBlock
    ensure: [ 
      "install old version of registry"
      newRegistry := self registry.	"see https://github.com/dalehenrich/metacello-work/issues/210"
      self registry: oldRegistry ].
  self registry: newRegistry	"if control returned, install newRegistry"
]

{ #category : #mutability }
MetacelloProjectRegistration class >> copyRegistryWhile: aBlock [
    "install copy of registry for duration of <aBlock> execution."

    "registrations will be copied on write during <aBlock> execution."

    "Unconditionally revert to the original
	version of the registry. Otherwise leave the new copy installed."

    | oldRegistry newRegistry |
    oldRegistry := self registry.
    newRegistry := self registry copy.
    self registry: newRegistry.
    aBlock
        ensure: [ 
            "install old version of registry"
            self registry: oldRegistry ]
]

{ #category : #'instance creation' }
MetacelloProjectRegistration class >> fromMCBaselineProjectSpec: aProjectSpec [
    ^ self new
        projectName: aProjectSpec name;
        baselineProjectSpec: aProjectSpec;
        yourself
]

{ #category : #'instance creation' }
MetacelloProjectRegistration class >> fromMCConfigurationProjectSpec: aProjectSpec [
    ^ self new
        projectName: aProjectSpec name;
        configurationProjectSpec: aProjectSpec;
        yourself
]

{ #category : #accessing }
MetacelloProjectRegistration class >> primeRegistryFromImage [
    "MetacelloProjectRegistration primeRegistryFromImage"

    self registry primeRegistryFromImage
]

{ #category : #querying }
MetacelloProjectRegistration class >> projectSpecForClassNamed: aClassName ifAbsent: absentBlock [
    ^ self registry projectSpecForClassNamed: aClassName ifAbsent: absentBlock
]

{ #category : #accessing }
MetacelloProjectRegistration class >> projectSpecs [
    "MetacelloProjectRegistration projectSpecs"

    ^ self configurationProjectSpecs , self baselineProjectSpecs
]

{ #category : #registration }
MetacelloProjectRegistration class >> registerProjectSpec: aProjectSpec ifPresent: presentBlock [
    | newRegistration |
    newRegistration := aProjectSpec asProjectRegistration.
    ^ self registry
        registrationFor: newRegistration
        ifPresent: [ :existing | presentBlock value: existing value: newRegistration ]
        ifAbsent: [ newRegistration registerProject ]
]

{ #category : #querying }
MetacelloProjectRegistration class >> registrationForClassNamed: aClassName ifAbsent: absentBlock [
    ^ self registry registrationForClassNamed: aClassName ifAbsent: absentBlock
]

{ #category : #querying }
MetacelloProjectRegistration class >> registrationForProjectSpec: aProjectSpec ifAbsent: absentBlock ifPresent: presentBlock [
    | newRegistration |
    newRegistration := aProjectSpec asProjectRegistration.
    self registry
        registrationFor: newRegistration
        ifPresent: [ :existing | ^ presentBlock value: existing value: newRegistration ]
        ifAbsent: [ ^ absentBlock value: newRegistration ]
]

{ #category : #accessing }
MetacelloProjectRegistration class >> registry [
    Registry ifNil: [ Registry := MetacelloProjectRegistry new ].
    ^ Registry
]

{ #category : #accessing }
MetacelloProjectRegistration class >> registry: aMetacelloProjectRegistry [
    Registry := aMetacelloProjectRegistry
]

{ #category : #accessing }
MetacelloProjectRegistration class >> resetRegistry [
    Registry := nil
]

{ #category : #comparing }
MetacelloProjectRegistration >> = aRegistration [
    aRegistration class == self class
        ifFalse: [ ^ false ].
    ^ (configurationProjectSpec registrationsCompareEqual: aRegistration configurationProjectSpec)
        and: [ baselineProjectSpec registrationsCompareEqual: aRegistration baselineProjectSpec ]
]

{ #category : #accessing }
MetacelloProjectRegistration >> baseName [
	^ self projectSpec baseName
]

{ #category : #accessing }
MetacelloProjectRegistration >> baselineProjectSpec [
  "only one of baselineProjectSpec or configurationProjectSpec should ever be set"

  ^ baselineProjectSpec
]

{ #category : #accessing }
MetacelloProjectRegistration >> baselineProjectSpec: anObject [
  "force the registration to be consistent -- difficult for Metacello to 
   repair registrations during load -- I've tried. "

  "https://github.com/dalehenrich/metacello-work/issues/212"

  self shouldBeMutable.
  configurationProjectSpec := nil.
  self assert: anObject isBaselineOfProjectSpec.
  baselineProjectSpec := anObject
]

{ #category : #accessing }
MetacelloProjectRegistration >> baselineProjectSpecIfAbsent: absentBlock [
  ^ baselineProjectSpec ifNil: absentBlock
]

{ #category : #accessing }
MetacelloProjectRegistration >> baselineProjectSpecIfPresent: presentBlock ifAbsent: absentBlock [
     ^ baselineProjectSpec ifNotNil: [ presentBlock cull: baselineProjectSpec ]  ifNil: absentBlock
]

{ #category : #testing }
MetacelloProjectRegistration >> canDowngradeTo: aProjectRegistration [
    "true if there are no load conflicts
        OR
       if the load conflicts involved two cofigurations ONLY and a downgrade is allowed"

    (self hasLoadConflicts: aProjectRegistration)
        ifFalse: [ ^ true ].
    configurationProjectSpec
        ifNotNil: [ 
            aProjectRegistration configurationProjectSpec
                ifNotNil: [ 
                    configurationProjectSpec ensureProjectLoaded.
                    ^ configurationProjectSpec canDowngradeTo: aProjectRegistration configurationProjectSpec ] ].
    ^ false
]

{ #category : #testing }
MetacelloProjectRegistration >> canUpgradeTo: aProjectRegistration [
  "true if there are no load conflicts
        OR
       if the load conflicts involved two cofigurations ONLY and an upgrade is allowed"

  (self hasLoadConflicts: aProjectRegistration)
    ifFalse: [ ^ true ].
  configurationProjectSpec
    ifNotNil: [ 
      aProjectRegistration configurationProjectSpec
        ifNotNil: [ 
          configurationProjectSpec copy ensureProjectLoaded.
          ^ configurationProjectSpec
            canUpgradeTo: aProjectRegistration configurationProjectSpec ] ].
  baselineProjectSpec
    ifNotNil: [ 
      aProjectRegistration baselineProjectSpec
        ifNotNil: [ 
          baselineProjectSpec copy ensureProjectLoaded.
          ^ baselineProjectSpec
            canUpgradeTo: aProjectRegistration baselineProjectSpec ] ].
  ^ false
]

{ #category : #accessing }
MetacelloProjectRegistration >> configurationProjectSpec [
  "only one of baselineProjectSpec or configurationProjectSpec should ever be set"

  ^ configurationProjectSpec
]

{ #category : #accessing }
MetacelloProjectRegistration >> configurationProjectSpec: anObject [
  "force the registration to be consistent -- difficult for Metacello to 
   repair registrations during load -- I've tried. "

  "https://github.com/dalehenrich/metacello-work/issues/212"

  self shouldBeMutable.
  baselineProjectSpec := nil.
  self assert: anObject isConfigurationOfProjectSpec.
  configurationProjectSpec := anObject
]

{ #category : #accessing }
MetacelloProjectRegistration >> configurationProjectSpecIfAbsent: absentBlock [
    ^ configurationProjectSpec ifNil: absentBlock
]

{ #category : #accessing }
MetacelloProjectRegistration >> configurationProjectSpecIfPresent: presentBlock ifAbsent: absentBlock [
    ^ configurationProjectSpec ifNotNil: [ presentBlock cull: configurationProjectSpec ]  ifNil: absentBlock
]

{ #category : #mutability }
MetacelloProjectRegistration >> copyOnWrite: aBlock [
    "assume that only registered projects are immutable ... otherwise you'll get an error"

    | copy |
    self class registry
        registrationFor: self
        ifPresent: [ :existing |  ]
        ifAbsent: [ 
            aBlock value: self.
            ^ self ].
    self unregisterProject.
    copy := self copy.
    aBlock value: copy.
    copy registerProject.
    ^ copy
]

{ #category : #accessing }
MetacelloProjectRegistration >> currentBranchName [
  ^ self configurationProjectSpec
    ifNotNil: [ 
      configurationProjectSpec versionOrNil
        ifNil: [ '' ]
        ifNotNil: [ :vrsn | vrsn blessing asString ] ]
    ifNil: [ baselineProjectSpec repositoryBranchName ]
]

{ #category : #accessing }
MetacelloProjectRegistration >> currentVersionString [
  ^ self configurationProjectSpec
    ifNotNil: [ :aSpec | aSpec versionString ]
    ifNil: [ baselineProjectSpec repositoryVersionString ]
]

{ #category : #querying }
MetacelloProjectRegistration >> currentlyLoadedClassesInProject [
	^ self projectSpec currentlyLoadedClassesInVersion asSet
]

{ #category : #testing }
MetacelloProjectRegistration >> hasLoadConflicts: aProjectRegistration [
	"5 combinations of loads with no load conflicts:
        No configs and baselines =
        configs = and no baselines
        configs = and baselines =
        configs = and no baseline loaded (self) with a baseline to load (aProjectRegistration)
        config loaded (self), no config to load (aProjectRegistration) and no baseline loaded(self) with a baseline to load (aProjectRegistration) "

	aProjectRegistration validate.
	self isValid
		ifFalse: [ ^ false ].
	^ self projectSpec hasConflictWithProjectSpec: aProjectRegistration projectSpec
]

{ #category : #comparing }
MetacelloProjectRegistration >> hash [
    ^ ((String stringHash: projectName initialHash: 0) bitXor: configurationProjectSpec metacelloRegistrationHash)
        bitXor: baselineProjectSpec metacelloRegistrationHash
]

{ #category : #mutability }
MetacelloProjectRegistration >> immutable [
    mutable := false
]

{ #category : #mutability }
MetacelloProjectRegistration >> isMutable [
    mutable ifNil: [ ^ true ].
    ^ mutable
]

{ #category : #testing }
MetacelloProjectRegistration >> isValid [

	" has a name and one or the other of the projectSpecs is non-nil, but not both ... this is CRITICAL"

	projectName ifNil: [ ^ false ].
	configurationProjectSpec ifNil: [ ^ baselineProjectSpec notNil ].
	^ baselineProjectSpec isNil
]

{ #category : #accessing }
MetacelloProjectRegistration >> loadedInImage [
    loadedInImage ifNil: [ loadedInImage := false ].
    ^ loadedInImage
]

{ #category : #accessing }
MetacelloProjectRegistration >> loadedInImage: anObject [
    self shouldBeMutable.
    loadedInImage := anObject
]

{ #category : #accessing }
MetacelloProjectRegistration >> locked [
    locked ifNil: [ locked := false ].
    ^ locked
]

{ #category : #accessing }
MetacelloProjectRegistration >> locked: anObject [
    self shouldBeMutable.
    locked := anObject
]

{ #category : #lookup }
MetacelloProjectRegistration >> lookupBaselineSpec [
  baselineProjectSpec ifNotNil: [ :spec | ^ spec ].
  ^ configurationProjectSpec
]

{ #category : #lookup }
MetacelloProjectRegistration >> lookupConfigurationSpec [
  configurationProjectSpec ifNotNil: [ :spec | ^ spec ].
  ^ baselineProjectSpec
]

{ #category : #accessing }
MetacelloProjectRegistration >> merge: aProjectRegistration [
  " ... merge is done when a spec has been loaded into the image"

  self shouldBeMutable.
  aProjectRegistration validate.
  aProjectRegistration configurationProjectSpec
    ifNotNil: [ :aSpec |
      configurationProjectSpec := aSpec copy.
      configurationProjectSpec versionOrNil
        ifNotNil: [ :version | 
          "resolve symbolic versions for loaded projects"
          configurationProjectSpec versionString: version versionString ].
      baselineProjectSpec := nil.
      self versionInfo versionString: configurationProjectSpec versionString ]
    ifNil: [ 
      baselineProjectSpec := aProjectRegistration baselineProjectSpec.
      configurationProjectSpec := nil.
      self versionInfo
        versionString: baselineProjectSpec repositoryVersionString ]
]

{ #category : #mutability }
MetacelloProjectRegistration >> mutable [
    mutable := true
]

{ #category : #copying }
MetacelloProjectRegistration >> postCopy [
    super postCopy.
    mutable := nil
]

{ #category : #printing }
MetacelloProjectRegistration >> printOn: aStream [
  | label versionString descriptions |
  self
    configurationProjectSpecIfPresent: [ :spec | 
      label := spec className.
      versionString := spec versionString ]
    ifAbsent: [ 
      "baseline"
      label := self baselineProjectSpec className.
      versionString := '[baseline]' ].
  aStream nextPutAll: label.
  versionString
    ifNil: [ aStream nextPutAll: ' --no version specified--' ]
    ifNotNil: [ 
      aStream
        space;
        nextPutAll: versionString ].
  (descriptions := self repositoryDescriptions) isEmpty
    ifTrue: [ ^ self ].
  aStream nextPutAll: ' from '.
  descriptions size = 1
    ifTrue: [ aStream nextPutAll: descriptions first ]
    ifFalse: [ 
      aStream nextPut: ${.
      descriptions do: [ :description | aStream nextPutAll: description ].
      aStream nextPut: $} ]
]

{ #category : #accessing }
MetacelloProjectRegistration >> projectName [
	^ projectName
]

{ #category : #accessing }
MetacelloProjectRegistration >> projectName: anObject [
    self shouldBeMutable.
    projectName := anObject
]

{ #category : #accessing }
MetacelloProjectRegistration >> projectSpec [
	^ baselineProjectSpec
		ifNil: [ 
			self assert: configurationProjectSpec notNil.
			configurationProjectSpec ]
]

{ #category : #accessing }
MetacelloProjectRegistration >> projectVersion [
  | pv |
  pv := self versionInfo projectVersion.
  pv
    ifNil: [ 
      pv := self projectSpec versionOrNil.
      self versionInfo projectVersion: pv ].
  ^ pv
]

{ #category : #registration }
MetacelloProjectRegistration >> registerProject [
    "unconditionally register <newRegistration> ... use with care"

    self class registry registerProjectRegistration: self
]

{ #category : #accessing }
MetacelloProjectRegistration >> repositoryDescriptions [
    ^ (self configurationProjectSpecIfAbsent: [ self baselineProjectSpec ]) repositoryDescriptions
]

{ #category : #mutability }
MetacelloProjectRegistration >> shouldBeMutable [
    self isMutable
        ifTrue: [ ^ self ].
    self error: 'Not allowed to modify an immutable object'
]

{ #category : #registration }
MetacelloProjectRegistration >> unregisterProject [
    self class registry unregisterProjectRegistration: self
]

{ #category : #accessing }
MetacelloProjectRegistration >> validate [
	self isValid
		ifFalse: [ self error: 'Invalid project registration' ]
]

{ #category : #accessing }
MetacelloProjectRegistration >> version [
    ^ (self configurationProjectSpecIfAbsent: [ ^ MetacelloMCBaselineProject singletonVersionName ]) versionString
]

{ #category : #accessing }
MetacelloProjectRegistration >> versionInfo [
  versionInfo
    ifNil: [ 
      versionInfo := MetacelloProjectRegistrationVersionInfo new.
      self
        configurationProjectSpecIfPresent: [ versionInfo versionString: self version ]
        ifAbsent: [ 
          "do not set versionString for freshly minted baseline ... 
           see https://github.com/dalehenrich/metacello-work/issues/328"
           ] ].
  ^ versionInfo
]

{ #category : #accessing }
MetacelloProjectRegistration >> versionInfo: aMetacelloProjectRegistrationVersionInfo [
  versionInfo := aMetacelloProjectRegistrationVersionInfo
]
