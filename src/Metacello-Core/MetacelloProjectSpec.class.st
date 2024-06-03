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
		'className',
		'projectInitialized'
	],
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

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

{ #category : 'enumerating' }
MetacelloProjectSpec >> currentlyLoadedClassesInVersion [

	self versionOrNil ifNotNil: [ :vrsn |
		^ vrsn spec currentlyLoadedClassesInVersion ].
	^ #(  )
]

{ #category : 'loading' }
MetacelloProjectSpec >> ensureProjectLoadedWithEngine: anEngine [
	"Ensure that the MetacelloProject is loaded in image. 
	 projectClass == nil or requested version non-existent warrants a project package load."

	(projectInitialized and: [
		 (self isMutable and: [ anEngine ignoreImage ]) not ]) ifTrue: [
		^ self ].

	projectInitialized := true.
	project := anEngine loader loadProject: self
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

{ #category : 'initialization' }
MetacelloProjectSpec >> initialize [

	super initialize.
	projectInitialized := false
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

{ #category : 'printing' }
MetacelloProjectSpec >> label [

	^self name
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
	aSpec loads ifNotNil: [ :otherLoads |
		self loads
			ifNil: [ loads := otherLoads ]
			ifNotNil: [ loads := (loads , otherLoads) asSet asArray ] ]
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

{ #category : 'visiting' }
MetacelloProjectSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [
    projectBlock value: self
]

{ #category : 'accessing' }
MetacelloProjectSpec >> projectInitialized [
	^ projectInitialized
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

	^ self className = aMetacelloProjectSpec className and: [
		  self versionString = aMetacelloProjectSpec versionString and: [
			  self operator == aMetacelloProjectSpec operator ] ]
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

{ #category : 'scripting' }
MetacelloProjectSpec >> validateForScriptLoad: aScriptEngine withDefaultVersionString: defaultVersionString withDefaultRepositoryDecription: defaultRepositoryDecription [

	| issues callSite |
	issues := OrderedCollection new.
	callSite := #validateForScriptLoad:withDefaultVersionString:withDefaultRepositoryDecription:.
	self name ifNil: [
		issues add: (MetacelloValidationError
				 reasonCode: #incompleteProjectSpec
				 callSite: callSite
				 explanation: 'name field required') ].
	self className ifNil: [
		issues add: (MetacelloValidationError
				 reasonCode: #incompleteProjectSpec
				 callSite: callSite
				 explanation: 'className field required') ].
	self repositories isEmpty ifTrue: [
		defaultRepositoryDecription
			ifNotNil: [ self repository: defaultRepositoryDecription ]
			ifNil: [
				issues add: (MetacelloValidationError
						 reasonCode: #incompleteProjectSpec
						 callSite: callSite
						 explanation: 'repository field required') ] ].
	self
		validateVersionString: issues
		withDefaultVersionString: defaultVersionString.
	^ issues
]

{ #category : 'scripting' }
MetacelloProjectSpec >> validateVersionString: issues withDefaultVersionString: defaultVersionString [

	self versionString ifNil: [
		defaultVersionString
			ifNotNil: [ self versionString: defaultVersionString ]
			ifNil: [
				issues add: (MetacelloValidationError
						 reasonCode: #incompleteProjectSpec
						 callSite:
						 #validateForScriptLoad:withDefaultVersionString:withDefaultRepositoryDecription:
						 explanation: 'version field required') ] ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> version [
	"Empty version string means use latestVersion or #bleedingEdge"

	^ self versionString
		  ifNotNil: [ self project versionString ]
		  ifNil: [
			  | vrsn |
			  "Eventually it will become an error to not specify a project reference version as default: #stable is the preferred default""self deprecated: 'Must specify a project reference version.'."
			  self flag: 'deprecate after version 1.0'.
			  (vrsn := self project latestVersion) isNil
				  ifTrue: [ self project version: #bleedingEdge ]
				  ifFalse: [ vrsn ] ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> versionKey [
	"suitable for using as a unique key for the receiver's version in a dictionary"

	^ self version versionKey
]

{ #category : 'querying' }
MetacelloProjectSpec >> versionOrNil [

	^ [ self version ]
		  on: MetacelloVersionDoesNotExistError
		  do: [ :ex | nil ]
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
