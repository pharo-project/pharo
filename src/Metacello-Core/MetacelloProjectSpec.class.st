Class {
	#name : 'MetacelloProjectSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
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

{ #category : 'class initialization' }
MetacelloProjectSpec class >> initialize [

	"Deprecation added during the development of Pharo 14."
	self deprecatedAliases:
		#( MetacelloMCBaselineOfProjectSpec MetacelloConfigurationOfProjectSpec MetacelloCypressBaselineProjectSpec MetacelloMCNamelessProjectSpec
		   MetacelloNamelessProjectSpec MetacelloBaselineOfProjectSpec )
]

{ #category : 'visiting' }
MetacelloProjectSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitProjectSpec: self
]

{ #category : 'adding' }
MetacelloProjectSpec >> addToMetacelloPackages: aMetacelloPackagesSpec [

	| spec |
	spec := aMetacelloPackagesSpec project projectReferenceSpec
		        name: self name;
		        projectReference: self copy;
		        yourself.
	aMetacelloPackagesSpec addMemberForSpec: spec
]

{ #category : 'scripting' }
MetacelloProjectSpec >> asProjectRegistration [

	^ MetacelloProjectRegistration projectSpec: self
]

{ #category : 'scripting' }
MetacelloProjectSpec >> asProjectSpec [
    ^ self
]

{ #category : 'accessing' }
MetacelloProjectSpec >> baseName [
	^ MetacelloScriptEngine baseNameOf: self className
]

{ #category : 'scripting' }
MetacelloProjectSpec >> canUpgradeTo: aProjectSpec [
	"cannot upgrade between baselines unless repositories can be upgraded"

	(self repositories canUpgradeTo: aProjectSpec repositories) ifTrue: [ ^ true ].
	^ false
]

{ #category : 'querying' }
MetacelloProjectSpec >> className [

	className ifNil: [ self name ifNotNil: [ self className: self constructClassName ] ].
	^ className
]

{ #category : 'accessing' }
MetacelloProjectSpec >> className: aString [
	"We can remove this check in a few version of Pharo. It was added in the development of Pharo 14."

	aString ifNotNil: [ (aString beginsWith: 'ConfigurationOf') ifTrue: [ self error: 'Configurations are not supported anymore' ] ].
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
							  self repositories compareEqual: aMetacelloProjectSpec repositories ] ] ] ] ] ]
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
MetacelloProjectSpec >> configMethodBodyOn: aStream indent: indent [
  ^ self configMethodBodyOn: aStream indent: indent fromShortCut: false
]

{ #category : 'printing' }
MetacelloProjectSpec >> configMethodBodyOn: aStream indent: indent fromShortCut: fromShortCut [

	| hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
	hasClassName := self hasClassName.
	hasOperator := operator isNotNil.
	hasProjectPackage := self hasRepository or: [ hasClassName ].
	hasLoads := self loads isNotNil.
	hasPreLoadDoIt := self preLoadDoIt isNotNil.
	hasPostLoadDoIt := self postLoadDoIt isNotNil.
	hasClassName ifTrue: [
			hasOperator | hasProjectPackage | hasLoads
				ifTrue: [
						aStream
							cr;
							tab: indent + 1 ]
				ifFalse: [ aStream space ].
			aStream nextPutAll: 'className: ' , self className printString.
			hasPreLoadDoIt | hasPostLoadDoIt | hasOperator | hasLoads | hasProjectPackage ifTrue: [ aStream nextPut: $; ] ].
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
		hasClassName | hasProjectPackage | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
			ifTrue: [
				aStream
					cr;
					tab: indent + 1 ]
			ifFalse: [ aStream space ].
		aStream nextPutAll: 'operator: #' , self operator asString printString.
		hasProjectPackage | hasLoads ifTrue: [ aStream nextPut: $; ] ].
	hasLoads ifTrue: [
		hasClassName | hasOperator | hasProjectPackage | hasPreLoadDoIt | hasPostLoadDoIt
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
		hasName := self name ~= self className.
		hasName ifTrue: [
			hasClassName | hasOperator | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
				ifTrue: [
						aStream
							cr;
							tab: indent + 1 ]
				ifFalse: [ aStream space ].
			aStream nextPutAll: 'file: ' , self name printString.
			hasRepo ifTrue: [ aStream nextPut: $; ] ].
		hasRepo ifTrue: [
			| repos |
			repos := self repositories map values.
			repos size = 1
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
					hasClassName | hasOperator | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
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
											hasClassName | hasOperator | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt | hasName
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

	| hasOperator hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
	hasClassName := self hasClassName.
	hasOperator := operator isNotNil.
	hasProjectPackage := self hasRepository or: [ hasClassName & (className ~= self name) ].
	hasLoads := self loads isNotNil.
	hasPreLoadDoIt := self preLoadDoIt isNotNil.
	hasPostLoadDoIt := self postLoadDoIt isNotNil.
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
			^ self ]
]

{ #category : 'private' }
MetacelloProjectSpec >> constructClassName [

	^ 'BaselineOf' , self name
]

{ #category : 'mutability' }
MetacelloProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [

	| copy |
	copy := aMetacelloProjectRegistration projectSpec copy.
	aBlock value: copy.
	aMetacelloProjectRegistration projectSpec: copy
]

{ #category : 'accessing' }
MetacelloProjectSpec >> currentlyLoadedClassesInVersion [

	self versionOrNil ifNotNil: [ :vrsn | ^ vrsn spec currentlyLoadedClassesInVersion ].
	^ #(  )
]

{ #category : 'loading' }
MetacelloProjectSpec >> ensureProjectLoadedWithEngine: anEngine [
	"Ensure that the MetacelloProject is loaded in image. 
	 projectClass == nil or requested version non-existent warrants a project package load."

	(projectInitialized and: [ (self isMutable and: [ anEngine ignoreImage ]) not ]) ifTrue: [ ^ self ].

	projectInitialized := true.
	project := anEngine loader loadProject: self
]

{ #category : 'accessing' }
MetacelloProjectSpec >> getClassName [
    "raw access to iv"

    ^ className
]

{ #category : 'accessing' }
MetacelloProjectSpec >> getRepositories [
    "raw access to iv"

    ^ repositories
]

{ #category : 'printing' }
MetacelloProjectSpec >> hasClassName [

	^ className isNotNil and: [ className ~= self constructClassName ]
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasLoadConflicts: aMetacelloProjectSpec [
  ^ (self hasNoLoadConflicts: aMetacelloProjectSpec) not
]

{ #category : 'scripting' }
MetacelloProjectSpec >> hasNoLoadConflicts: aMetacelloProjectSpec [
	"'projectPackage repositories'"

	^ self className = aMetacelloProjectSpec className and: [
			  (self compareVersionsEqual: aMetacelloProjectSpec) and: [
					  self operator == aMetacelloProjectSpec operator and: [
						  (self repositories isEmpty or: [ aMetacelloProjectSpec repositories isEmpty ]) or: [
							  self repositories hasNoLoadConflicts: aMetacelloProjectSpec repositories ] ] ] ]
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasNonVersionStringField [

	| hasProjectPackage |
	self hasClassName ifTrue: [ ^ true ].

	self versionString ifNotNil: [ ^ true ].

	operator ifNotNil: [ ^ true ].

	hasProjectPackage := self className ~= self name or: [ self hasRepository ].
	hasProjectPackage ifTrue: [ ^ true ].

	self loads ifNotNil: [ ^ true ].

	self preLoadDoIt ifNotNil: [ ^ true ].
	self postLoadDoIt ifNotNil: [ ^ true ].

	^ false
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

{ #category : 'scripting' }
MetacelloProjectSpec >> lookForIssues [

	self name ifNil: [ MetacelloValidationFailure signal: 'Name field required' ].
	self className ifNil: [ MetacelloValidationFailure signal: 'ClassName field required' ]
]

{ #category : 'importing' }
MetacelloProjectSpec >> mergeImportLoads: aLoadList [

	aLoadList ifNotNil: [ :otherLoads |
			self loads
				ifNil: [ loads := otherLoads ]
				ifNotNil: [ loads := loads , otherLoads ] ]
]

{ #category : 'merging' }
MetacelloProjectSpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #name put: name.
	map at: #className put: className.
	map at: #operator put: operator.
	map at: #loads put: loads.
	map at: #preLoadDoIt put: preLoadDoIt.
	map at: #postLoadDoIt put: postLoadDoIt.
	map at: #repositories put: repositories.
	^ map
]

{ #category : 'merging' }
MetacelloProjectSpec >> mergeRepositoriesSpec: anotherRepositories [
	"anotherRepositories wins ... there can ever only be one repository for the 
   baseline to load from"

	"https://github.com/dalehenrich/metacello-work/issues/251"

	self repositories: anotherRepositories
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
	hash := String stringHash: self operator asString initialHash: hash.
	hash := String stringHash: self preLoadDoIt asString initialHash: hash.
	hash := String stringHash: self postLoadDoIt asString initialHash: hash.
	hash := hash bitXor: loads hash.
	^ hash bitXor: self repositories metacelloRegistrationHash
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
MetacelloProjectSpec >> postLoadDoIt: aSymbol [

	self shouldBeMutable.
	self validateDoItSelector: aSymbol.
	postLoadDoIt := aSymbol
]

{ #category : 'querying' }
MetacelloProjectSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloProjectSpec >> preLoadDoIt: aSymbol [

	self shouldBeMutable.
	self validateDoItSelector: aSymbol.
	preLoadDoIt := aSymbol
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

	^ 'baseline'
]

{ #category : 'accessing' }
MetacelloProjectSpec >> projectPackage [
    projectPackage
        ifNil: [ 
            self className ifNil: [ ^ nil ].
            projectPackage := self project packageSpec.
            projectPackage name: self className.
            projectPackage repositories: self getRepositories ].
    ^ projectPackage
]

{ #category : 'accessing' }
MetacelloProjectSpec >> projectPackage: aProjectPackage [
    self shouldBeMutable.
    projectPackage := aProjectPackage
]

{ #category : 'querying' }
MetacelloProjectSpec >> projectReference [
	
	^ self
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

	^ self className = aMetacelloProjectSpec className and: [ self operator == aMetacelloProjectSpec operator ]
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repositories [

	^ repositories ifNil: [ repositories := self project repositoriesSpec ]
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

{ #category : 'querying' }
MetacelloProjectSpec >> repositoryVersionString [
	"extract a version string from the repository ... if possible"

	"must parallel implementation of MetacelloMCBaselineProject>>setBaselineRepositoryDescription: we want the same repoSpec"

	| spec repo |
	self repositorySpecs do: [ :repoSpec | spec := repoSpec ].
	[ repo := spec createRepository ]
		on: Error
		do: [ :ex | ^ '' ].
	^ repo repositoryVersionString
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

{ #category : 'scripting' }
MetacelloProjectSpec >> unregisterProject [
    ^ MetacelloProjectRegistration
        registrationForProjectSpec: self
        ifAbsent: [  ]
        ifPresent: [ :existing :new | existing unregisterProject ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> version [

	projectInitialized ifFalse: [ ^ nil ].
	^ self project version
]

{ #category : 'querying' }
MetacelloProjectSpec >> versionOrNil [

	^ [ self version ]
		  on: MetacelloVersionDoesNotExistError
		  do: [ :ex | nil ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> versionString [

	^ self version ifNotNil: [ :v | v versionString ]
]
