Class {
	#name : 'MetacelloProjectSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'file',
		'versionString',
		'preLoadDoIt',
		'loads',
		'projectPackage',
		'name',
		'repositories',
		'operator',
		'postLoadDoIt',
		'className'
	],
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'class initialization' }
MetacelloProjectSpec class >> initialize [

	self deprecatedAliases: { #MetacelloMCProjectSpec. #MetacelloGenericProjectSpec }
]

{ #category : 'instance creation' }
MetacelloProjectSpec class >> new [
	self name == #MetacelloProjectSpec
		ifTrue: [ self error: 'This class is abstract' ].
	^ super new
]

{ #category : 'adding' }
MetacelloProjectSpec >> addToMetacelloPackages: aMetacelloPackagesSpec [

	| spec |
	spec := (aMetacelloPackagesSpec project projectReferenceSpec)
			name: self name;
			projectReference: self copy;
			yourself.
	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec addMember 
			name: spec name;
			spec: spec;
			yourself)
]

{ #category : 'testing' }
MetacelloProjectSpec >> allPackagesLoaded: aLoader [
	"answer true if all of the packages (excluding projects) are loaded"

	| vrsn pkgs |
	(vrsn := self versionOrNil) ifNil: [ ^ false ].
	pkgs := OrderedCollection new.
	(self loadListForVersion: vrsn) do: [ :nm | vrsn packages do: [ :pkg | (pkg isPackageLoaded: aLoader) ifFalse: [ ^ false ] ] ].
	^ true
]

{ #category : 'scripting' }
MetacelloProjectSpec >> asBaselineProjectSpec [
    ^ self copyForScriptingInto: (MetacelloMCBaselineOfProjectSpec for: self project asBaselineProject)
]

{ #category : 'scripting' }
MetacelloProjectSpec >> asConfigurationProjectSpec [
    ^ self copyForScriptingInto: (MetacelloMCConfigurationOfProjectSpec for: self project asConfigurationProject)
]

{ #category : 'scripting' }
MetacelloProjectSpec >> asProjectRegistration [
  (self className beginsWith: 'BaselineOf')
    ifTrue: [ 
      ^ MetacelloProjectRegistration
        fromMCBaselineProjectSpec: self asBaselineProjectSpec ].
  ^ MetacelloProjectRegistration
    fromMCConfigurationProjectSpec: self asConfigurationProjectSpec
]

{ #category : 'scripting' }
MetacelloProjectSpec >> asProjectSpec [
    ^ self
]

{ #category : 'scripting' }
MetacelloProjectSpec >> asProjectSpecForVersion: vrsn [
  ^ self
]

{ #category : 'accessing' }
MetacelloProjectSpec >> baseName [
	^ MetacelloScriptEngine baseNameOf: self className
]

{ #category : 'scripting' }
MetacelloProjectSpec >> canDowngradeTo: aMetacelloProjectSpec [

	self file = aMetacelloProjectSpec file ifFalse: [ ^ false ].
	(self className = aMetacelloProjectSpec className and: [ self operator == aMetacelloProjectSpec operator ]) ifFalse: [ ^ false ].
	self versionOrNil ifNil: [ "https://github.com/dalehenrich/metacello-work/issues/198#issuecomment-21737458" ^ true ].
	^ (self compareVersions: aMetacelloProjectSpec usingOperator: self operator) not
]

{ #category : 'scripting' }
MetacelloProjectSpec >> canUpgradeTo: aMetacelloProjectSpec [

	self file = aMetacelloProjectSpec file ifFalse: [ ^ false ].
	(self className = aMetacelloProjectSpec className and: [ self operator == aMetacelloProjectSpec operator ]) ifFalse: [ ^ false ].
	self versionOrNil ifNil: [ "https://github.com/dalehenrich/metacello-work/issues/198#issuecomment-21737458" ^ true ].
	^ self compareVersions: aMetacelloProjectSpec usingOperator: self operator
]

{ #category : 'querying' }
MetacelloProjectSpec >> className [
    className ifNil: [ self name ifNotNil: [ self className: self constructClassName ] ].
    ^ className
]

{ #category : 'accessing' }
MetacelloProjectSpec >> className: aString [

	self shouldBeMutable.
	className := aString.
	self projectPackage: nil
]

{ #category : 'testing' }
MetacelloProjectSpec >> compareCurrentVersion: anOperator targetVersionStatus: targetVersionStatus using: anMCLoader [

	^ (MetacelloLookupProjectSpecForLoad new
		   projectSpec: self;
		   yourself) signal
		  performCurrentVersionTestAgainst: self versionOrNil
		  operator: anOperator
		  targetVersionStatus: targetVersionStatus
		  using: anMCLoader
]

{ #category : 'scripting' }
MetacelloProjectSpec >> compareEqual: aMetacelloProjectSpec [
	"'projectPackage repositories'"

	^ self className = aMetacelloProjectSpec className and: [
		  (self compareVersionsEqual: aMetacelloProjectSpec) and: [
			  self operator == aMetacelloProjectSpec operator and: [
				  self loads = aMetacelloProjectSpec loads and: [
					  self preLoadDoIt value == aMetacelloProjectSpec preLoadDoIt value and: [
						  self postLoadDoIt value == aMetacelloProjectSpec postLoadDoIt value and: [
							  (self repositories compareEqual: aMetacelloProjectSpec repositories) and: [ self file = aMetacelloProjectSpec file ] ] ] ] ] ] ]
]

{ #category : 'scripting' }
MetacelloProjectSpec >> compareVersions: aMetacelloProjectSpec usingOperator: anOperator [

	^ aMetacelloProjectSpec versionOrNil
		  ifNotNil: [ ^ aMetacelloProjectSpec version perform: anOperator with: self version ]
		  ifNil: [ "https://github.com/dalehenrich/metacello-work/issues/199#issuecomment-21739622"
			  aMetacelloProjectSpec versionString asMetacelloVersionNumber perform: anOperator with: self version versionNumber ]
]

{ #category : 'scripting' }
MetacelloProjectSpec >> compareVersionsEqual: aMetacelloProjectSpec [
  | vrsn otherVrsn |
  vrsn := self versionOrNil.
  otherVrsn := aMetacelloProjectSpec versionOrNil.
  vrsn ifNil: [ ^ vrsn = otherVrsn ].
  otherVrsn ifNil: [ ^ false ].
  ^ vrsn versionNumber = otherVrsn versionNumber
]

{ #category : 'printing' }
MetacelloProjectSpec >> configHasVersionString [

	^ self versionString isNotNil
]

{ #category : 'printing' }
MetacelloProjectSpec >> configMethodBodyOn: aStream indent: indent [
  ^ self configMethodBodyOn: aStream indent: indent fromShortCut: false
]

{ #category : 'printing' }
MetacelloProjectSpec >> configMethodBodyOn: aStream indent: indent fromShortCut: fromShortCut [

	| hasVersionString hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
	hasClassName := self hasClassName.
	hasVersionString := self configHasVersionString.
	hasOperator := operator isNotNil.
	hasProjectPackage := self hasRepository or: [ hasClassName & self getFile isNotNil ].
	hasLoads := self loads isNotNil.
	hasPreLoadDoIt := self getPreLoadDoIt isNotNil.
	hasPostLoadDoIt := self getPostLoadDoIt isNotNil.
	hasClassName ifTrue: [
		hasVersionString | hasOperator | hasProjectPackage | hasLoads
			ifTrue: [
				aStream
					cr;
					tab: indent + 1 ]
			ifFalse: [ aStream space ].
		aStream nextPutAll: 'className: ' , self className printString.
		hasVersionString | hasPreLoadDoIt | hasPostLoadDoIt | hasOperator | hasLoads | hasProjectPackage ifTrue: [ aStream nextPut: $; ] ].
	hasVersionString ifTrue: [
		| vs |
		hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
			ifTrue: [
				aStream
					cr;
					tab: indent + 1 ]
			ifFalse: [ aStream space ].
		vs := self versionString.
		aStream nextPutAll: 'versionString: '.
		vs isSymbol ifTrue: [ aStream nextPut: $# ].
		aStream nextPutAll: vs asString printString.
		hasPreLoadDoIt | hasPostLoadDoIt | hasOperator | hasProjectPackage | hasLoads ifTrue: [ aStream nextPut: $; ] ].
	hasPreLoadDoIt ifTrue: [
		hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt
			ifTrue: [
				aStream
					cr;
					tab: indent + 1 ]
			ifFalse: [ aStream space ].
		aStream nextPutAll: 'preLoadDoIt: '.
		self preLoadDoIt value isSymbol
			ifTrue: [
				aStream
					nextPut: $#;
					nextPutAll: self preLoadDoIt value asString printString ]
			ifFalse: [ aStream nextPutAll: self preLoadDoIt value asString ].
		hasPostLoadDoIt | hasOperator | hasProjectPackage | hasLoads ifTrue: [ aStream nextPut: $; ] ].
	hasPostLoadDoIt ifTrue: [
		hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPostLoadDoIt
			ifTrue: [
				aStream
					cr;
					tab: indent + 1 ]
			ifFalse: [ aStream space ].
		aStream nextPutAll: 'postLoadDoIt: '.
		self postLoadDoIt value isSymbol
			ifTrue: [
				aStream
					nextPut: $#;
					nextPutAll: self postLoadDoIt value asString printString ]
			ifFalse: [ aStream nextPutAll: self postLoadDoIt value asString ].
		hasOperator | hasProjectPackage | hasLoads ifTrue: [ aStream nextPut: $; ] ].
	hasOperator ifTrue: [
		hasClassName | hasVersionString | hasProjectPackage | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
			ifTrue: [
				aStream
					cr;
					tab: indent + 1 ]
			ifFalse: [ aStream space ].
		aStream nextPutAll: 'operator: #' , self operator asString printString.
		hasProjectPackage | hasLoads ifTrue: [ aStream nextPut: $; ] ].
	hasLoads ifTrue: [
		hasClassName | hasVersionString | hasOperator | hasProjectPackage | hasPreLoadDoIt | hasPostLoadDoIt
			ifTrue: [
				aStream
					cr;
					tab: indent + 1 ]
			ifFalse: [ aStream space ].
		aStream nextPutAll: 'loads: #('.
		self loads do: [ :str | aStream nextPutAll: str printString , ' ' ].
		aStream nextPut: $).
		hasProjectPackage ifTrue: [ aStream nextPut: $; ] ].
	hasProjectPackage ifTrue: [
		| hasName hasRepo |
		hasRepo := self hasRepository.
		hasName := self file ~= self className.
		hasName ifTrue: [
			hasClassName | hasVersionString | hasOperator | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
				ifTrue: [
					aStream
						cr;
						tab: indent + 1 ]
				ifFalse: [ aStream space ].
			aStream nextPutAll: 'file: ' , self file printString.
			hasRepo ifTrue: [ aStream nextPut: $; ] ].
		hasRepo ifTrue: [
			| repos |
			repos := self repositories map values.
			repos size = 1
				ifTrue: [
					fromShortCut
						ifTrue: [
							hasClassName | hasVersionString | hasOperator | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt | hasName
								ifTrue: [
									aStream
										cr;
										tab: indent + 1 ]
								ifFalse: [ aStream space ] ]
						ifFalse: [
							aStream
								cr;
								tab: indent + 1 ].
					repos first configMethodCascadeOn: aStream lastCascade: true ]
				ifFalse: [
					aStream cr.
					self repositories configMethodCascadeOn: aStream indent: indent ] ] ]
]

{ #category : 'printing' }
MetacelloProjectSpec >> configMethodOn: aStream indent: indent [

	aStream 
		tab: indent;
		nextPutAll: 'spec '; cr;
		tab: indent + 1;
		nextPutAll: 'name: ', self name printString, ';'.
	self configMethodBodyOn: aStream indent: indent.
	aStream nextPut: $.
]

{ #category : 'printing' }
MetacelloProjectSpec >> configShortCutMethodOn: aStream member: aMember indent: indent [

	| hasVersionString hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
	hasClassName := self hasClassName.
	hasVersionString := self configHasVersionString.
	hasOperator := operator isNotNil.
	hasProjectPackage := self hasRepository or: [ hasClassName & (self getFile isNotNil or: [ className ~= self name ]) ].
	hasLoads := self loads isNotNil.
	hasPreLoadDoIt := self getPreLoadDoIt isNotNil.
	hasPostLoadDoIt := self getPostLoadDoIt isNotNil.
	hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt ifTrue: [
		(aMember methodUpdateSelector == #copy: or: [ aMember methodUpdateSelector == #with: ])
			ifTrue: [
				aStream
					nextPutAll: 'with: [';
					cr ]
			ifFalse: [
				aStream
					nextPutAll: 'overrides: [';
					cr ].
		aStream
			tab: indent;
			nextPutAll: 'spec'.
		self configMethodBodyOn: aStream indent: indent fromShortCut: true.
		aStream nextPutAll: ' ]'.
		^ self ].
	hasVersionString ifTrue: [
		| vs |
		vs := self versionString.
		aStream nextPutAll: 'with: '.
		vs isSymbol ifTrue: [ aStream nextPut: $# ].
		aStream nextPutAll: vs asString printString ]
]

{ #category : 'private' }
MetacelloProjectSpec >> constructClassName [
    ^ nil
]

{ #category : 'mutability' }
MetacelloProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [
    self subclassResponsibility
]

{ #category : 'scripting' }
MetacelloProjectSpec >> copyForScriptingInto: aProjectSpec [
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

{ #category : 'querying' }
MetacelloProjectSpec >> currentlyLoadedClassesInVersion [
    self versionOrNil ifNotNil: [ :vrsn | ^ vrsn spec currentlyLoadedClassesInVersion ].
    ^ #()
]

{ #category : 'loading' }
MetacelloProjectSpec >> determineCurrentVersionForLoad [
	"don't use self currentVersion, because we are interested in the currentVersion of the project as loaded in image, not the current version relative to our load list"

	| prjct version currentVersion cvs |
	self projectClass ifNil: [ ^ nil ].
	(version := self versionOrNil) ifNil: [ ^ nil ].
	version blessing == #baseline ifTrue: [ ^ version ].
	self loader ignoreImage ifTrue: [ ^ version ].
	prjct := self projectClass new project.
	prjct loader: self loader.
	(currentVersion := prjct currentVersion) ifNil: [ ^ nil ].
	(cvs := currentVersion versionStatus) == #somethingLoaded ifTrue: [ ^ nil ].
	(#( #allLoadedToSpec #loadedToSpec #loadedMatchConstraints ) includes: (cvs := currentVersion versionStatus)) ifTrue: [
		(currentVersion perform: self operator with: version) ifTrue: [ "load currentVersion" ^ currentVersion ]. "load version"
		^ nil ].
	version = currentVersion ifTrue: [ ^ currentVersion ]. "I don't believe that it is possible to reach this point in the method, so I will be interested if I run across a case that produces this error"
	(MetacelloProjectSpecLoadConflict projectSpec: self) signal:
		'Project load conflict for' , prjct label printString , ' between current version ' , currentVersion printString , '(' , cvs asString
		, ') and specified version ' , version printString , '. Press resume to continue with load anyway'.
	^ nil
]

{ #category : 'loading' }
MetacelloProjectSpec >> ensureConfigurationLoaded: vrsn ensured: ensured [
  "answer true if the configuration should be reloaded:
    blessing is #development
    symbolic version (https://github.com/dalehenrich/metacello-work/issues/283)"

  (vrsn blessing == #'development' or: [ self versionString isSymbol ])
    ifTrue: [ 
      ensured ~~ #'latest'
        ifTrue: [ ^ MetacelloScriptEnsureProjectLoadedForDevelopment signal ] ].
  ^ false
]

{ #category : 'loading' }
MetacelloProjectSpec >> ensureLoadUsing: aLoader [
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
  pp forceLoadUsing: aLoader
]

{ #category : 'loading' }
MetacelloProjectSpec >> ensureLoadedForDevelopmentUsing: mcLoader [
  "for #development projects, always need latest version of package when contemplating a load"
  | ensured |

	self isLocked ifTrue: [ 
		mcLoader ensuredMap at: self name put: #'present'.
		^ self ].

	ensured := mcLoader ensuredMap at: self name ifAbsent: [ nil ].	
	self projectClass ifNotNil: [
		self ensureProjectLoaded: mcLoader ensured: ensured.
		^ self ].

  ensured ifNil: [ 
	self ensureProjectFetch: mcLoader ]
]

{ #category : 'private - loading' }
MetacelloProjectSpec >> ensureProjectFetch: mcLoader [

	"projectClass == nil or version == nil"
	mcLoader ensureForDevelopment 
		ifTrue: [ 
			self notifyCleanStackCache.
			self ensureLoadUsing: mcLoader ]
		ifFalse: [ 
			self fetchUsing: mcLoader ].
		
	mcLoader ensuredMap 
		at: self name 
		put: #'present'
]

{ #category : 'loading' }
MetacelloProjectSpec >> ensureProjectLoaded [
	"Ensure that the MetacelloProject is loaded in image. 
	 projectClass == nil or requested version non-existent warrants a project package load."

	"answer true if the projectClass exists"

	(self projectClass isNil or: [ self versionOrNil isNil or: [ (loader notNil or: [ self isMutable ]) and: [ self loader ignoreImage ] ] ]) ifTrue: [
		self projectClass ifNotNil: [ :pc |
			MetacelloClearStackCacheNotification signal: #( #currentVersion #currentVersionAgainst: #currentVersionInfo #versionConstructor #loadableSpecNames ) , { pc } ].
		self projectPackage ifNil: [ ^ true ].
		self ensureLoadUsing: self loader ].
	^ self projectClass isNotNil
]

{ #category : 'private - loading' }
MetacelloProjectSpec >> ensureProjectLoaded: mcLoader ensured: ensured [
	| version |

	version := self versionOrNil.
	version ifNil: [ ^ self ].
	(self ensureConfigurationLoaded: version ensured: ensured)  ifFalse: [ ^ self ].
	
	mcLoader ensureForDevelopment
		ifTrue: [ 
			self notifyCleanStackCache.
			self ensureLoadUsing: mcLoader ]
		ifFalse: [ 
			self projectPackage fetchUsing: mcLoader ].
	mcLoader ensuredMap at: self name put: #'latest'
]

{ #category : 'loading' }
MetacelloProjectSpec >> fetchUsing: aLoader [
    (MetacelloLookupProjectSpec new
        projectSpec: self;
        yourself) signal projectPackage fetchUsing: aLoader
]

{ #category : 'querying' }
MetacelloProjectSpec >> file [

	^ file ifNil: [ ^ self className ]
]

{ #category : 'accessing' }
MetacelloProjectSpec >> file: aString [
    self shouldBeMutable.
    file := aString.
    self projectPackage: nil
]

{ #category : 'accessing' }
MetacelloProjectSpec >> getClassName [
    "raw access to iv"

    ^ className
]

{ #category : 'accessing' }
MetacelloProjectSpec >> getFile [
    "raw access to iv"

    ^ file
]

{ #category : 'accessing' }
MetacelloProjectSpec >> getOperator [

	^operator
]

{ #category : 'querying' }
MetacelloProjectSpec >> getPostLoadDoIt [

	^postLoadDoIt
]

{ #category : 'querying' }
MetacelloProjectSpec >> getPreLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloProjectSpec >> getRepositories [
    "raw access to iv"

    ^ repositories
]

{ #category : 'printing' }
MetacelloProjectSpec >> hasClassName [

	^ className isNotNil
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasConflictWithBaselineSpec: projectSpec [
	^ self hasLoadConflicts: projectSpec
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasConflictWithConfigurationSpec: projectSpec [
	^ self hasLoadConflicts: projectSpec
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasConflictWithProjectSpec: projectSpec [
  (self className beginsWith: 'BaselineOf')
    ifTrue: [ ^ projectSpec hasConflictWithBaselineSpec: self asBaselineProjectSpec ].
  ^ projectSpec
    hasConflictWithConfigurationSpec: self asConfigurationProjectSpec
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasLoadConflicts: aMetacelloProjectSpec [
  ^ (self hasNoLoadConflicts: aMetacelloProjectSpec) not
]

{ #category : 'scripting' }
MetacelloProjectSpec >> hasNoLoadConflicts: aMetacelloProjectSpec [
	"'projectPackage repositories'"

	^ (self className = aMetacelloProjectSpec className and: [
		   (self compareVersionsEqual: aMetacelloProjectSpec) and: [
			   self operator == aMetacelloProjectSpec operator and: [
				   (self repositories isEmpty or: [ aMetacelloProjectSpec repositories isEmpty ]) or: [
					   self repositories hasNoLoadConflicts: aMetacelloProjectSpec repositories ] ] ] ]) and: [ self file = aMetacelloProjectSpec file ]
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasNonVersionStringField [

	| hasVersionString hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
	hasClassName := self hasClassName.
	hasVersionString := self versionString isNotNil.
	hasOperator := operator isNotNil.
	hasProjectPackage := (self file isNotNil and: [ hasClassName and: [ self className ~= self name ] ]) or: [ self hasRepository ].
	hasLoads := self loads isNotNil.
	hasPreLoadDoIt := self getPreLoadDoIt isNotNil.
	hasPostLoadDoIt := self getPostLoadDoIt isNotNil.
	^ hasClassName | hasOperator | hasProjectPackage | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasRepository [
    ^ self repositorySpecs notEmpty
]

{ #category : 'testing' }
MetacelloProjectSpec >> isBaselineOfProjectSpec [
	^ false
]

{ #category : 'testing' }
MetacelloProjectSpec >> isConfigurationOfProjectSpec [
	^ false
]

{ #category : 'scripting' }
MetacelloProjectSpec >> isLocked [

  ^ self registration 
	ifNotNil: [ :aRegistration | aRegistration locked ]
	ifNil: [ false ]
]

{ #category : 'testing' }
MetacelloProjectSpec >> isPartiallyLoaded: aLoader [

	| vrsn |
	(vrsn := self versionOrNil) ifNil: [ ^ false ].
	(self loadListForVersion: vrsn) do: [ :nm | (vrsn packagesForSpecNamed: nm) do: [ :pkg | (pkg isPackageLoaded: aLoader) ifTrue: [ ^ true ] ] ].
	^ false
]

{ #category : 'printing' }
MetacelloProjectSpec >> label [

	^self name
]

{ #category : 'loading' }
MetacelloProjectSpec >> load [

    (MetacelloLookupProjectSpecForLoad new
        projectSpec: self;
        yourself) signal performLoad
]

{ #category : 'private' }
MetacelloProjectSpec >> loadListForVersion: vrsn [

	^ (self loads isNil or: [ self loads isEmpty ])
		  ifTrue: [ vrsn spec defaultPackageNames ]
		  ifFalse: [ self loads ]
]

{ #category : 'accessing' }
MetacelloProjectSpec >> loadPackageList [

	| vrsn pkgs |
	(vrsn := self versionOrNil) ifNil: [ ^ #(  ) ].
	pkgs := OrderedCollection new.
	(self loadListForVersion: vrsn) do: [ :nm | pkgs addAll: ((vrsn packagesForSpecNamed: nm) collect: [ :each | each name ]) ].
	^ pkgs
]

{ #category : 'private - loading' }
MetacelloProjectSpec >> loadProjectIsLocked: version [
	"Just inform is already loaded"

	MetacelloProjectSpecLoadedNotification new
		projectSpec: (self copy versionString: version versionString);
		signal
]

{ #category : 'loading' }
MetacelloProjectSpec >> loadVersion: aVersionOrNil [
	"Load the correct version of the project"

	| vrsn mcLoader list |
	self ensureProjectLoaded.
	vrsn := aVersionOrNil.
	vrsn ifNil: [
		[ vrsn := self version ]
			on: MetacelloVersionDoesNotExistError
			do: [ :ex |
				^ (MetacelloProjectSpecLoadError projectSpec: self)
					  versionDoesNotExistException: ex;
					  signal: 'No version found for ' , self versionString printString , ' of ' , self className asString , ' because: ' , ex description ] ].
	mcLoader := self loader copy.
	mcLoader operator: self operator.
	vrsn loader: mcLoader.
	list := (mcLoader ignoreImage
		         ifTrue: [ self loadListForVersion: vrsn ]
		         ifFalse: [ vrsn packageAndProjectNamesToLoad: (self loadListForVersion: vrsn) loader: mcLoader ]) asSet.
	MetacelloPlatform current
		useStackCacheDuring: [ :dict |
			| projectCache |
			projectCache := dict at: self projectClass ifAbsent: [ dict at: self projectClass put: Dictionary new ].
			(projectCache at: vrsn ifAbsent: [  ])
				ifNotNil: [ :cachedList |
					(cachedList size = list size and: [ cachedList allSatisfy: [ :each | list includes: each ] ])
						ifTrue: [ "no need to refetch list ... recursion stoppper (Issue 95)" ^ self ]
						ifFalse: [ projectCache at: vrsn put: list ] ]
				ifNil: [ projectCache at: vrsn put: list ].
			vrsn versionString ~= self versionString ifTrue: [ MetacelloNotification signal: ' [' , vrsn versionString , ']' ].
			mcLoader preLoad: self.
			vrsn fetchRequiredFromArray: list. "do the load"
			(MetacelloProjectSpecLoadedNotification new projectSpec: (self copy versionString: vrsn versionString)) signal.
			mcLoader postLoad: self ]
		defaultDictionary: Dictionary new
]

{ #category : 'accessing' }
MetacelloProjectSpec >> loadedPackageNames: aLoader [

	| vrsn pkgs |
	(vrsn := self versionOrNil) ifNil: [ ^ #(  ) ].
	pkgs := OrderedCollection new.
	(self loadListForVersion: vrsn) do: [ :nm | (vrsn packagesForSpecNamed: nm) do: [ :pkg | (pkg isPackageLoaded: aLoader) ifTrue: [ pkgs add: pkg name ] ] ].
	^ pkgs
]

{ #category : 'querying' }
MetacelloProjectSpec >> loads [
	^ loads
]

{ #category : 'accessing' }
MetacelloProjectSpec >> loads: aCollection [

	aCollection setLoadsInMetacelloProject: self
]

{ #category : 'merging' }
MetacelloProjectSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #name put: name.
	map at: #className put: className.
	map at: #versionString put: versionString.
	map at: #operator put: operator.
	map at: #loads put: loads.
	map at: #preLoadDoIt put: preLoadDoIt.
	map at: #postLoadDoIt put: postLoadDoIt.
	map at: #repositories put: repositories.
	^ map
]

{ #category : 'merging' }
MetacelloProjectSpec >> mergeRepositoriesSpec: anotherRepositories [

	self repositories: (self getRepositories
			 ifNotNil: [ self repositories mergeSpec: anotherRepositories ]
			 ifNil: [ anotherRepositories ])
]

{ #category : 'scripting' }
MetacelloProjectSpec >> mergeScriptLoads: aSpec [
    self shouldBeMutable.
    aSpec loads
        ifNotNil: [ :otherLoads | self loads ifNil: [ loads := otherLoads ] ifNotNil: [ loads := (loads , otherLoads) asSet asArray ] ].
    self loader: aSpec loader
]

{ #category : 'scripting' }
MetacelloProjectSpec >> mergeScriptRepository: anotherSpec [
    self repositories: anotherSpec repositories
]

{ #category : 'merging' }
MetacelloProjectSpec >> mergeSpec: anotherSpec [

	| newSpec map |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	(map at: #repositories) ifNotNil: [ :anotherRepositories | newSpec mergeRepositoriesSpec: anotherRepositories ].
	^ newSpec
]

{ #category : 'scripting' }
MetacelloProjectSpec >> metacelloRegistrationHash [
	"file"

	| hash |
	hash := String stringHash: name initialHash: 0.
	hash := String stringHash: self className initialHash: hash.
	hash := String stringHash: self versionString initialHash: hash.
	hash := String stringHash: self operator asString initialHash: hash.
	hash := String stringHash: self preLoadDoIt asString initialHash: hash.
	hash := String stringHash: self postLoadDoIt asString initialHash: hash.
	hash := hash bitXor: loads hash.
	hash := hash bitXor: self repositories metacelloRegistrationHash.
	^ String stringHash: self file initialHash: hash
]

{ #category : 'querying' }
MetacelloProjectSpec >> name [

	^name
]

{ #category : 'accessing' }
MetacelloProjectSpec >> name: aString [

	((aString at: 1) isSeparator or: [ (aString at: aString size) isSeparator ]) ifTrue: [
		self error: 'Names are not allowed to have leading or trailing blanks: ' , aString printString ].
	self shouldBeMutable.
	name := aString.
	self projectPackage: nil
]

{ #category : 'merging' }
MetacelloProjectSpec >> nonOverridable [
    ^ super nonOverridable , #(#'projectPackage' #'repositories')
]

{ #category : 'private - loading' }
MetacelloProjectSpec >> notifyCleanStackCache [

	self projectClass ifNotNil: [ :pc |
		MetacelloClearStackCacheNotification signal: #( #currentVersion #currentVersionAgainst: #currentVersionInfo #versionConstructor #loadableSpecNames ) , { pc } ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> operator [

	^ operator ifNil: [ #'>=' ]
]

{ #category : 'accessing' }
MetacelloProjectSpec >> operator: anObject [
    " #= #~= #> #< #>= #<= #~> "

    self shouldBeMutable.
    operator := anObject
]

{ #category : 'accessing' }
MetacelloProjectSpec >> packageFileSpecFor: aMetacelloPackagesSpec [

	^(aMetacelloPackagesSpec project projectReferenceSpec)
			name: self name;
			projectReference: self copy;
			yourself.
]

{ #category : 'copying' }
MetacelloProjectSpec >> postCopy [
    super postCopy.
    repositories := repositories copy.
    projectPackage := nil
]

{ #category : 'querying' }
MetacelloProjectSpec >> postLoadDoIt [

	^postLoadDoIt
]

{ #category : 'accessing' }
MetacelloProjectSpec >> postLoadDoIt: anObject [

	anObject setPostLoadDoItInMetacelloSpec: self
]

{ #category : 'querying' }
MetacelloProjectSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloProjectSpec >> preLoadDoIt: anObject [

	anObject setPreLoadDoItInMetacelloSpec: self
]

{ #category : 'querying' }
MetacelloProjectSpec >> projectClass [

	self className ifNil: [ ^ nil ].
	^ Smalltalk at: self className asSymbol ifAbsent: [  ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> projectClassProject [

	"Initialize the project object in the baseline/configuration"
	| projectClassInstance |
	projectClassInstance := self projectClass new.
	projectClassInstance initalizeProjectWithRepositoryDescription:
		self repositoryDescriptions.

	"Then return it through the project method to call the hook redefined in subclasses for compatibility.
	Subclasses typically change the load type of the project to #atomic"
	^ projectClassInstance project
]

{ #category : 'visiting' }
MetacelloProjectSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [
    projectBlock value: self
]

{ #category : 'printing' }
MetacelloProjectSpec >> projectLabel [
    ^ 'project'
]

{ #category : 'accessing' }
MetacelloProjectSpec >> projectPackage [
    projectPackage
        ifNil: [ 
            self className ifNil: [ ^ nil ].
            projectPackage := self project packageSpec.
            projectPackage name: self className.
            self getFile ifNotNil: [ projectPackage file: self file ].
            projectPackage repositories: self getRepositories ].
    ^ projectPackage
]

{ #category : 'accessing' }
MetacelloProjectSpec >> projectPackage: aProjectPackage [
    self shouldBeMutable.
    projectPackage := aProjectPackage
]

{ #category : 'scripting' }
MetacelloProjectSpec >> registration [
    ^ MetacelloProjectRegistration
        registrationForProjectSpec: self
        ifAbsent: [ :ignored |  ]
        ifPresent: [ :existing :new | existing ]
]

{ #category : 'scripting' }
MetacelloProjectSpec >> registrationsCompareEqual: aMetacelloProjectSpec [
    "name className versionString operator loads preLoadDoIt postLoadDoIt"

    ^ self className = aMetacelloProjectSpec className
        and: [ self versionString = aMetacelloProjectSpec versionString and: [ self operator == aMetacelloProjectSpec operator ] ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> relativeCurrentVersion [
	"currentVersion calculated relative to the loadList"

	| vrsn expanded loadList |
	(vrsn := self versionOrNil) ifNil: [ ^ nil ].
	expanded := [ vrsn expandToLoadableSpecNames: (loadList := self loadListForVersion: vrsn) ]
		            on: Error
		            do: [ :ex |
			            vrsn blessing == #development ifTrue: [
				            self ensureLoadUsing: self loader.
				            vrsn := self versionOrNil.
				            ex return: (vrsn expandToLoadableSpecNames: loadList) ].
			            ex pass ].
	^ self projectClassProject currentVersionAgainst: expanded
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repositories [
    repositories ifNil: [ repositories := self project repositoriesSpec ].
    ^ repositories
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repositories: anObject [
    self shouldBeMutable.
    repositories := anObject.
    self projectPackage: nil
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repository: aStringOrMetacelloRepositorySpec [
    self repositories repository: aStringOrMetacelloRepositorySpec.
    self projectPackage: nil
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repository: aString username: username password: password [
    self repositories repository: aString username: username password: password.
    self projectPackage: nil
]

{ #category : 'querying' }
MetacelloProjectSpec >> repositoryDescriptions [
    ^ self repositorySpecs collect: [ :repoSpec | repoSpec description ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> repositorySpecs [
    repositories ifNil: [ ^ #() ].
    ^ self repositories map values
]

{ #category : 'private' }
MetacelloProjectSpec >> resolveToLoadableSpec [

	^self copy
]

{ #category : 'private' }
MetacelloProjectSpec >> setLoads: aCollection [
    self shouldBeMutable.
    loads := aCollection
]

{ #category : 'private' }
MetacelloProjectSpec >> setName: aStringOrNil [
    self shouldBeMutable.
    name := aStringOrNil
]

{ #category : 'accessing' }
MetacelloProjectSpec >> setPostLoadDoIt: aSymbol [
    self shouldBeMutable.
    postLoadDoIt := aSymbol
]

{ #category : 'accessing' }
MetacelloProjectSpec >> setPreLoadDoIt: aSymbol [
    self shouldBeMutable.
    preLoadDoIt := aSymbol
]

{ #category : 'scripting' }
MetacelloProjectSpec >> unregisterProject [
    ^ MetacelloProjectRegistration
        registrationForProjectSpec: self
        ifAbsent: [  ]
        ifPresent: [ :existing :new | existing unregisterProject ]
]

{ #category : 'development support' }
MetacelloProjectSpec >> updateForSpawnMethod: sourceSpec [
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"

	repositories := className := operator := loads := projectPackage := nil.
	sourceSpec ifNotNil: [ versionString := sourceSpec versionString ]
]

{ #category : 'development support' }
MetacelloProjectSpec >> updatePackageSpec: updatedSpecs [
	"Add project copy to updatedSpecs if the current version of the project 
	 is different from the receiver's version"

	| prj currentVersion spec |
	className ifNil: [ ^ self ].
	prj := self projectClassProject.
	(currentVersion := prj currentVersion) = self versionOrNil ifTrue: [ ^ self ].
	currentVersion ifNil: [ ^ self ].
	spec := self copy.
	spec versionString: currentVersion versionString.
	updatedSpecs at: spec name put: spec
]

{ #category : 'scripting' }
MetacelloProjectSpec >> validateForScriptLoad: aScriptEngine withDefaultVersionString: defaultVersionString withDefaultRepositoryDecription: defaultRepositoryDecription [
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

{ #category : 'scripting' }
MetacelloProjectSpec >> validateVersionString: issues withDefaultVersionString: defaultVersionString [
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

{ #category : 'querying' }
MetacelloProjectSpec >> version [
	"Empty version string means use latestVersion or #bleedingEdge"

	self projectClass ifNil: [ ^ nil ].
	^ self versionString
		  ifNotNil: [ self projectClassProject version: self versionString ]
		  ifNil: [
			  | vrsn |
			  "Eventually it will become an error to not specify a project reference version as default: #stable is the preferred default""self deprecated: 'Must specify a project reference version.'."
			  self flag: 'deprecate after version 1.0'.
			  (vrsn := self projectClassProject latestVersion) isNil
				  ifTrue: [ self projectClassProject version: #bleedingEdge ]
				  ifFalse: [ vrsn ] ]
]

{ #category : 'scripting' }
MetacelloProjectSpec >> versionForScriptEngine: aMetacelloScriptEngine [
    | prj |
    prj := self projectClass ifNil: [ self project ] ifNotNil: [ self projectClassProject ].
    ^ ((prj projectForScriptEngine: aMetacelloScriptEngine) version: self versionString)
        silently: aMetacelloScriptEngine silently;
        ignoreImage: aMetacelloScriptEngine ignoreImage
]

{ #category : 'querying' }
MetacelloProjectSpec >> versionKey [
	"suitable for using as a unique key for the receiver's version in a dictionary"

	^ self version versionKey
]

{ #category : 'querying' }
MetacelloProjectSpec >> versionOrNil [

	^[ self version ] on: MetacelloVersionDoesNotExistError do: [:ex | ^nil ].
]

{ #category : 'querying' }
MetacelloProjectSpec >> versionString [
	^ versionString
]

{ #category : 'accessing' }
MetacelloProjectSpec >> versionString: anObject [
    self shouldBeMutable.
    versionString := anObject
]

{ #category : 'accessing' }
MetacelloProjectSpec >> workingCopy [
  ^ self projectPackage workingCopy
]
