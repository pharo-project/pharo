Class {
	#name : #MetacelloMCProjectSpec,
	#superclass : #MetacelloGenericProjectSpec,
	#instVars : [
		'file'
	],
	#category : #'Metacello-MC-Specs'
}

{ #category : #'instance creation' }
MetacelloMCProjectSpec class >> new [
	self name == #MetacelloMCProjectSpec
		ifTrue: [ self error: 'This class is abstract' ].
	^ super new
]

{ #category : #testing }
MetacelloMCProjectSpec >> allPackagesLoaded: aLoader [
    "answer true if all of the packages (excluding projects) are loaded"

    | vrsn pkgs |
    (vrsn := self versionOrNil) == nil
        ifTrue: [ ^ false ].
    pkgs := OrderedCollection new.
    (self loadListForVersion: vrsn)
        do: [ :nm | 
            vrsn packages
                do: [ :pkg | 
                    (pkg isPackageLoaded: aLoader)
                        ifFalse: [ ^ false ] ] ].
    ^ true
]

{ #category : #scripting }
MetacelloMCProjectSpec >> asBaselineProjectSpec [
    ^ self copyForScriptingInto: (MetacelloMCBaselineOfProjectSpec for: self project asBaselineProject)
]

{ #category : #scripting }
MetacelloMCProjectSpec >> asConfigurationProjectSpec [
    ^ self copyForScriptingInto: (MetacelloMCConfigurationOfProjectSpec for: self project asConfigurationProject)
]

{ #category : #scripting }
MetacelloMCProjectSpec >> asProjectRegistration [
  (self className beginsWith: 'BaselineOf')
    ifTrue: [ 
      ^ MetacelloProjectRegistration
        fromMCBaselineProjectSpec: self asBaselineProjectSpec ].
  ^ MetacelloProjectRegistration
    fromMCConfigurationProjectSpec: self asConfigurationProjectSpec
]

{ #category : #scripting }
MetacelloMCProjectSpec >> asProjectSpec [
    ^ self
]

{ #category : #scripting }
MetacelloMCProjectSpec >> asProjectSpecForVersion: vrsn [
  ^ self
]

{ #category : #accessing }
MetacelloMCProjectSpec >> baseName [
	^ MetacelloScriptEngine baseNameOf: self className
]

{ #category : #scripting }
MetacelloMCProjectSpec >> canDowngradeTo: aMetacelloProjectSpec [
    ^ (super canDowngradeTo: aMetacelloProjectSpec) and: [ self file = aMetacelloProjectSpec file ]
]

{ #category : #scripting }
MetacelloMCProjectSpec >> canUpgradeTo: aMetacelloProjectSpec [
    ^ (super canUpgradeTo: aMetacelloProjectSpec) and: [ self file = aMetacelloProjectSpec file ]
]

{ #category : #accessing }
MetacelloMCProjectSpec >> className: aString [
    super className: aString.
    self projectPackage: nil
]

{ #category : #testing }
MetacelloMCProjectSpec >> compareCurrentVersion: anOperator targetVersionStatus: targetVersionStatus using: anMCLoader [
    ^ (MetacelloLookupProjectSpecForLoad new
        projectSpec: self;
        yourself) signal
        performCurrentVersionTestAgainst: self versionOrNil
        operator: anOperator
        targetVersionStatus: targetVersionStatus
        using: anMCLoader
]

{ #category : #scripting }
MetacelloMCProjectSpec >> compareEqual: aMetacelloProjectSpec [
    "'projectPackage repositories'"

    ^ (super compareEqual: aMetacelloProjectSpec) and: [ self file = aMetacelloProjectSpec file ]
]

{ #category : #testing }
MetacelloMCProjectSpec >> compareRelativeCurrentVersion: anOperator targetVersionStatus: targetVersionStatus using: anMCLoader [

	| cv vrsn |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^false ].
	(cv := self relativeCurrentVersion) == nil ifTrue: [ ^false ].
	(targetVersionStatus includes: cv versionStatus)
		ifTrue: [ ^cv perform: anOperator with: vrsn ].
	^false
]

{ #category : #mutability }
MetacelloMCProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [
    self subclassResponsibility
]

{ #category : #scripting }
MetacelloMCProjectSpec >> copyForScriptingInto: aProjectSpec [
    ^aProjectSpec
        setName: name;
        className: className;
        versionString: versionString;
        operator: operator;
        setLoads: loads;
        preLoadDoIt: preLoadDoIt;
        postLoadDoIt: postLoadDoIt;
        repositories: repositories copy;
        file: file
]

{ #category : #loading }
MetacelloMCProjectSpec >> determineCurrentVersionForLoad [
    "don't use self currentVersion, because we are interested in the currentVersion of the project as loaded in image, not the current version relative to our load list"

    | prjct version currentVersion cvs |
    self projectClass == nil
        ifTrue: [ ^ nil ].
    (version := self versionOrNil) == nil
        ifTrue: [ ^ nil ].
    version blessing == #'baseline'
        ifTrue: [ ^ version ].
    self loader ignoreImage
        ifTrue: [ ^ version ].
    prjct := self projectClass new project.
    prjct loader: self loader.
    (currentVersion := prjct currentVersion) == nil
        ifTrue: [ ^ nil ].
    (cvs := currentVersion versionStatus) == #'somethingLoaded'
        ifTrue: [ ^ nil ].
    (#(#'allLoadedToSpec' #'loadedToSpec' #'loadedMatchConstraints') includes: (cvs := currentVersion versionStatus))
        ifTrue: [ 
            (currentVersion perform: self operator with: version)
                ifTrue: [ 
                    "load currentVersion"
                    ^ currentVersion ].	"load version"
            ^ nil ].
    version = currentVersion
        ifTrue: [ ^ currentVersion ].	"I don't believe that it is possible to reach this point in the method, so I will be interested if I run across a case that produces this error"
    (MetacelloProjectSpecLoadConflict projectSpec: self)
        signal:
            'Project load conflict for' , prjct label printString , ' between current version ' , currentVersion printString , '('
                , cvs asString , ') and specified version ' , version printString
                , '. Press resume to continue with load anyway'.
    ^ nil

]

{ #category : #loading }
MetacelloMCProjectSpec >> ensureConfigurationLoaded: vrsn ensured: ensured [
  "answer true if the configuration should be reloaded:
    blessing is #development
    symbolic version (https://github.com/dalehenrich/metacello-work/issues/283)"

  (vrsn blessing == #'development' or: [ self versionString isSymbol ])
    ifTrue: [ 
      ensured ~~ #'latest'
        ifTrue: [ ^ MetacelloScriptEnsureProjectLoadedForDevelopment signal ] ].
  ^ false
]

{ #category : #loading }
MetacelloMCProjectSpec >> ensureLoadUsing: aLoader [
  | pp |
  pp := (MetacelloLookupProjectSpec new
    projectSpec: self;
    yourself) signal projectPackage.
  pp
    ifNil: [ 
      self
        error:
          'Unable to resolve project package for ' , self name printString
            ,
              '. It is likely that that the configuration referencing this project will not validate properly (see MetacelloToolBox class>>validateConfiguration:).' ].
  pp ensureLoadUsing: aLoader
]

{ #category : #loading }
MetacelloMCProjectSpec >> ensureLoadedForDevelopmentUsing: mcLoader [
  "for #development projects, always need latest version of package when contemplating a load"

  | ensured |
  ensured := mcLoader ensuredMap at: self name ifAbsent: [ nil ].
  self projectClass ~~ nil
    ifTrue: [ 
      | vrsn |
      vrsn := self versionOrNil.
      vrsn ~~ nil
        ifTrue: [ 
          (self ensureConfigurationLoaded: vrsn ensured: ensured)
            ifTrue: [ 
              mcLoader ensureForDevelopment
                ifTrue: [ 
                  | pc |
                  (pc := self projectClass) ~~ nil
                    ifTrue: [ 
                      MetacelloClearStackCacheNotification
                        signal:
                          #(#'currentVersion' #'currentVersionAgainst:' #'currentVersionInfo' #'versionConstructor' #'loadableSpecNames')
                            , {pc} ].
                  self ensureLoadUsing: mcLoader ]
                ifFalse: [ self projectPackage fetchUsing: mcLoader ].
              mcLoader ensuredMap at: self name put: #'latest' ].
          ^ self ] ].
  ensured == nil
    ifTrue: [ 
      "projectClass == nil or version == nil"
      mcLoader ensureForDevelopment
        ifTrue: [ 
          | pc |
          (pc := self projectClass) ~~ nil
            ifTrue: [ 
              MetacelloClearStackCacheNotification
                signal:
                  #(#'currentVersion' #'currentVersionAgainst:' #'currentVersionInfo' #'versionConstructor' #'loadableSpecNames')
                    , {pc} ].
          self ensureLoadUsing: mcLoader ]
        ifFalse: [ self fetchUsing: mcLoader ].
      mcLoader ensuredMap at: self name put: #'present' ]
]

{ #category : #loading }
MetacelloMCProjectSpec >> ensureProjectLoaded [
  "Ensure that the MetacelloProject is loaded in image. 
	 projectClass == nil or requested version non-existent warrants a project package load."

  "answer true if the projectClass exists"

  (self projectClass == nil
    or: [ 
      self versionOrNil == nil
        or: [ (loader notNil or: [ self isMutable ]) and: [ self loader ignoreImage ] ] ])
    ifTrue: [ 
      | pc |
      (pc := self projectClass) ~~ nil
        ifTrue: [ 
          MetacelloClearStackCacheNotification
            signal:
              #(#'currentVersion' #'currentVersionAgainst:' #'currentVersionInfo' #'versionConstructor' #'loadableSpecNames')
                , {pc} ].
      self projectPackage ifNil: [ ^ true ].
      self ensureLoadUsing: self loader ].
  ^ self projectClass ~~ nil
]

{ #category : #loading }
MetacelloMCProjectSpec >> fetchUsing: aLoader [
    (MetacelloLookupProjectSpec new
        projectSpec: self;
        yourself) signal projectPackage fetchUsing: aLoader
]

{ #category : #querying }
MetacelloMCProjectSpec >> file [
    file ifNil: [ ^ self className ].
    ^ file
]

{ #category : #accessing }
MetacelloMCProjectSpec >> file: aString [
    self shouldBeMutable.
    file := aString.
    self projectPackage: nil
]

{ #category : #construction }
MetacelloMCProjectSpec >> file: aString constructor: aVersionConstructor [
    aVersionConstructor fileForProject: aString
]

{ #category : #accessing }
MetacelloMCProjectSpec >> getFile [
    "raw access to iv"

    ^ file
]

{ #category : #testing }
MetacelloMCProjectSpec >> hasConflictWithBaselineSpec: projectSpec [
	^ self hasLoadConflicts: projectSpec
]

{ #category : #testing }
MetacelloMCProjectSpec >> hasConflictWithConfigurationSpec: projectSpec [
	^ self hasLoadConflicts: projectSpec
]

{ #category : #testing }
MetacelloMCProjectSpec >> hasConflictWithProjectSpec: projectSpec [
  (self className beginsWith: 'BaselineOf')
    ifTrue: [ ^ projectSpec hasConflictWithBaselineSpec: self asBaselineProjectSpec ].
  ^ projectSpec
    hasConflictWithConfigurationSpec: self asConfigurationProjectSpec
]

{ #category : #scripting }
MetacelloMCProjectSpec >> hasNoLoadConflicts: aMetacelloProjectSpec [
  "'projectPackage repositories'"

  ^ (super hasNoLoadConflicts: aMetacelloProjectSpec)
    and: [ self file = aMetacelloProjectSpec file ]
]

{ #category : #testing }
MetacelloMCProjectSpec >> isBaselineOfProjectSpec [
	^ false
]

{ #category : #testing }
MetacelloMCProjectSpec >> isConfigurationOfProjectSpec [
	^ false
]

{ #category : #testing }
MetacelloMCProjectSpec >> isPartiallyLoaded: aLoader [

	| vrsn |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^false ].
	(self loadListForVersion: vrsn) do: [:nm |
		(vrsn packagesForSpecNamed: nm ) do: [:pkg |
			(pkg isPackageLoaded: aLoader) ifTrue: [ ^true ]]].
	^false
]

{ #category : #testing }
MetacelloMCProjectSpec >> isPossibleBaseline [

	| vrsn |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^false ].
	(vrsn allPackagesForSpecNamed: (self loadListForVersion: vrsn)) do: [:pkg |
		pkg workingCopy == nil ifTrue: [ ^false ]].
	^true
]

{ #category : #accessing }
MetacelloMCProjectSpec >> loadPackageList [

	| vrsn pkgs |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^#() ].
	pkgs := OrderedCollection new.
	(self loadListForVersion: vrsn) do: [:nm |
		pkgs addAll: ((vrsn packagesForSpecNamed: nm ) collect: [:each | each name ])].
	^pkgs
]

{ #category : #loading }
MetacelloMCProjectSpec >> loadVersion: aVersionOrNil [
	"Load the correct version of the project"

	| vrsn mcLoader list |
	self ensureProjectLoaded.
	vrsn := aVersionOrNil.
	vrsn == nil
		ifTrue: [ [ vrsn := self version ]
				on: MetacelloVersionDoesNotExistError
				do: [ :ex | 
					^ (MetacelloProjectSpecLoadError projectSpec: self)
						versionDoesNotExistException: ex;
						signal:
							'No version found for ' , self versionString printString , ' of '
								, self className asString , ' because: ' , ex description ] ].
	mcLoader := self loader copy.
	mcLoader operator: self operator.
	vrsn loader: mcLoader.
	list := (mcLoader ignoreImage
		ifTrue: [ self loadListForVersion: vrsn ]
		ifFalse: [ vrsn
				packageAndProjectNamesToLoad: (self loadListForVersion: vrsn)
				loader: mcLoader ]) asSet.
	MetacelloPlatform current
		useStackCacheDuring: [ :dict | 
			| projectCache cachedList |
			projectCache := dict
				at: self projectClass
				ifAbsent: [ dict at: self projectClass put: Dictionary new ].
			(cachedList := projectCache at: vrsn ifAbsent: [  ]) == nil
				ifTrue: [ projectCache at: vrsn put: list ]
				ifFalse: [ 
					(cachedList size = list size 
					and: [ cachedList allSatisfy: [ :each | list  includes: each ] ])
						ifTrue:
							[ "no need to refetch list ... recursion stoppper (Issue 95)" ^ self ]
						ifFalse: [ projectCache at: vrsn put: list ] ].
			vrsn versionString ~= self versionString
				ifTrue: [ MetacelloNotification signal: (' [' , vrsn versionString , ']') ].
			mcLoader preLoad: self.
			vrsn fetchRequiredFromArray: list.	"do the load"
			(MetacelloProjectSpecLoadedNotification new
				projectSpec: (self copy versionString: vrsn versionString)) signal.
			mcLoader postLoad: self ]
		defaultDictionary: Dictionary new
]

{ #category : #accessing }
MetacelloMCProjectSpec >> loadedPackageNames: aLoader [

	| vrsn pkgs |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^#() ].
	pkgs := OrderedCollection new.
	(self loadListForVersion: vrsn) do: [:nm |
		(vrsn packagesForSpecNamed: nm ) do: [:pkg |
			(pkg isPackageLoaded: aLoader) ifTrue: [ pkgs add: pkg name ]]].
	^pkgs
]

{ #category : #scripting }
MetacelloMCProjectSpec >> metacelloRegistrationHash [
    "file"

    ^ String stringHash: self file initialHash: super metacelloRegistrationHash
]

{ #category : #accessing }
MetacelloMCProjectSpec >> packageFileSpecFor: aMetacelloPackagesSpec [

	^(aMetacelloPackagesSpec project projectReferenceSpec)
			name: self name;
			projectReference: self copy;
			yourself.
]

{ #category : #querying }
MetacelloMCProjectSpec >> projectClass [
    self className == nil
        ifTrue: [ ^ nil ].
    ^ Smalltalk at: self className asSymbol ifAbsent: [  ]
]

{ #category : #querying }
MetacelloMCProjectSpec >> projectClassProject [
    "indirection needed when projectClass is _not_ a subclass of MetacelloProject"

    ^ self projectClass new project
        setBaselineRepositoryDescription: self repositoryDescriptions;
        yourself
]

{ #category : #accessing }
MetacelloMCProjectSpec >> projectPackage [
    projectPackage
        ifNil: [ 
            self className ifNil: [ ^ nil ].
            projectPackage := self project packageSpec.
            projectPackage name: self className.
            self getFile ifNotNil: [ projectPackage file: self file ].
            projectPackage repositories: self getRepositories ].
    ^ projectPackage
]

{ #category : #querying }
MetacelloMCProjectSpec >> relativeCurrentVersion [
    "currentVersion calculated relative to the loadList"

    | vrsn expanded loadList |
    (vrsn := self versionOrNil) == nil
        ifTrue: [ ^ nil ].
    expanded := [ vrsn expandToLoadableSpecNames: (loadList := self loadListForVersion: vrsn) ]
        on: Error
        do: [ :ex | 
            vrsn blessing == #'development'
                ifTrue: [ 
                    self ensureLoadUsing: self loader.
                    vrsn := self versionOrNil.
                    ex return: (vrsn expandToLoadableSpecNames: loadList) ].
            ex pass ].
    ^ self projectClassProject currentVersionAgainst: expanded
]

{ #category : #private }
MetacelloMCProjectSpec >> resolveToAllPackagesIn: aVersionSpec visited: visited [
    | vrsn |
    visited
        pushProject: [ 
            visited
                visit: self
                doing: [ :spec | 
                    spec ensureProjectLoaded.
                    vrsn := spec version.
                    ^ vrsn
                        allPackagesForSpecNamed: (self loadListForVersion: vrsn)
                        ifAbsent: [ self error: 'invalid loads: spec' ] ] ].
    ^ #()
]

{ #category : #'development support' }
MetacelloMCProjectSpec >> updateForSpawnMethod: sourceSpec [
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"
	
	repositories := className := operator := loads := projectPackage := nil.
	sourceSpec ~~ nil ifTrue: [ versionString := sourceSpec versionString ].
]

{ #category : #'development support' }
MetacelloMCProjectSpec >> updatePackageSpec: updatedSpecs [
    "Add project copy to updatedSpecs if the current version of the project 
	 is different from the receiver's version"

    | prj currentVersion spec |
    className == nil
        ifTrue: [ ^ self ].
    prj := self projectClassProject.
    (currentVersion := prj currentVersion) = self versionOrNil
        ifTrue: [ ^ self ].
    currentVersion == nil
        ifTrue: [ ^ self ].
    spec := self copy.
    spec versionString: currentVersion versionString.
    updatedSpecs at: spec name put: spec
]

{ #category : #scripting }
MetacelloMCProjectSpec >> validateForScriptLoad: aScriptEngine withDefaultVersionString: defaultVersionString withDefaultRepositoryDecription: defaultRepositoryDecription [
    | issues callSite |
    issues := OrderedCollection new.
    callSite := #'validateForScriptLoad:withDefaultVersionString:withDefaultRepositoryDecription:'.
    self name
        ifNil: [ 
            issues
                add:
                    (MetacelloValidationError
                        configurationClass: self projectClass
                        reasonCode: #'incompleteProjectSpec'
                        callSite: callSite
                        explanation: 'name field required') ].
    self className
        ifNil: [ 
            issues
                add:
                    (MetacelloValidationError
                        configurationClass: self projectClass
                        reasonCode: #'incompleteProjectSpec'
                        callSite: callSite
                        explanation: 'className field required') ].
    self repositories isEmpty
        ifTrue: [ 
            defaultRepositoryDecription
                ifNotNil: [ self repository: defaultRepositoryDecription ]
                ifNil: [ 
                    issues
                        add:
                            (MetacelloValidationError
                                configurationClass: self projectClass
                                reasonCode: #'incompleteProjectSpec'
                                callSite: callSite
                                explanation: 'repository field required') ] ].
    self validateVersionString: issues withDefaultVersionString: defaultVersionString.
    ^ issues
]

{ #category : #scripting }
MetacelloMCProjectSpec >> validateVersionString: issues withDefaultVersionString: defaultVersionString [
    self versionString
        ifNil: [ 
            defaultVersionString
                ifNotNil: [ self versionString: defaultVersionString ]
                ifNil: [ 
                    issues
                        add:
                            (MetacelloValidationError
                                configurationClass: self projectClass
                                reasonCode: #'incompleteProjectSpec'
                                callSite:
                                    #'validateForScriptLoad:withDefaultVersionString:withDefaultRepositoryDecription:'
                                explanation: 'version field required') ] ]
]

{ #category : #querying }
MetacelloMCProjectSpec >> version [
	"Empty version string means use latestVersion or #bleedingEdge"

	self projectClass == nil
		ifTrue: [ ^ nil ].
	^ self versionString == nil
		ifTrue: [ 
			| vrsn |
			"Eventually it will become an error to not specify a project reference version as default: #stable is the preferred default"
			"self deprecated: 'Must specify a project reference version.'."
			self flag: 'deprecate after version 1.0'.
			(vrsn := self projectClassProject latestVersion) == nil
				ifTrue: [ self projectClassProject version: #bleedingEdge ]
				ifFalse: [ vrsn ] ]
		ifFalse: [ self projectClassProject version: self versionString ]
]

{ #category : #scripting }
MetacelloMCProjectSpec >> versionForScriptEngine: aMetacelloScriptEngine [
    | prj |
    prj := self projectClass ifNil: [ self project ] ifNotNil: [ self projectClassProject ].
    ^ ((prj projectForScriptEngine: aMetacelloScriptEngine) version: self versionString)
        silently: aMetacelloScriptEngine silently;
        ignoreImage: aMetacelloScriptEngine ignoreImage;
        cacheRepository: aMetacelloScriptEngine cacheRepository;
        repositoryOverrides: aMetacelloScriptEngine repositoryOverrides
]

{ #category : #accessing }
MetacelloMCProjectSpec >> workingCopy [
  ^ self projectPackage workingCopy
]
