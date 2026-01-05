Class {
	#name : 'MetacelloProjectSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'project',
		'preLoadDoIt',
		'loads',
		'projectPackage',
		'name',
		'repositories',
		'postLoadDoIt',
		'className',
		'mutable'
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

{ #category : 'instance creation' }
MetacelloProjectSpec class >> named: aString [

	^ self new
		  name: aString;
		  yourself
]

{ #category : 'instance creation' }
MetacelloProjectSpec class >> project: aMetacelloProject [

	^ self new
		  project: aMetacelloProject;
		  yourself
]

{ #category : 'visiting' }
MetacelloProjectSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitProjectSpec: self
]

{ #category : 'adding' }
MetacelloProjectSpec >> addToMetacelloPackages: aMetacelloPackagesSpec [

	| spec |
	spec := MetacelloProjectReferenceSpec projectReference: self copy.
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

	self shouldBeMutable.
	className := aString.
	self resetProjectPackage
]

{ #category : 'scripting' }
MetacelloProjectSpec >> compareEqual: aMetacelloProjectSpec [
	"'projectPackage repositories'"

	^ self className = aMetacelloProjectSpec className and: [
			  (self hasVersion and: [ aMetacelloProjectSpec hasVersion ]) and: [
					  self loads = aMetacelloProjectSpec loads and: [
							  self preLoadDoIt == aMetacelloProjectSpec preLoadDoIt and: [
								  self postLoadDoIt == aMetacelloProjectSpec postLoadDoIt and: [ self repositories compareEqual: aMetacelloProjectSpec repositories ] ] ] ] ]
]

{ #category : 'printing' }
MetacelloProjectSpec >> configMethodBodyOn: aStream indent: indent [
  ^ self configMethodBodyOn: aStream indent: indent fromShortCut: false
]

{ #category : 'printing' }
MetacelloProjectSpec >> configMethodBodyOn: aStream indent: indent fromShortCut: fromShortCut [

	| hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
	hasClassName := self hasClassName.
	hasProjectPackage := self hasRepository or: [ hasClassName ].
	hasLoads := self loads isNotNil.
	hasPreLoadDoIt := self preLoadDoIt isNotNil.
	hasPostLoadDoIt := self postLoadDoIt isNotNil.
	hasClassName ifTrue: [
			hasProjectPackage | hasLoads
				ifTrue: [
						aStream
							cr;
							tab: indent + 1 ]
				ifFalse: [ aStream space ].
			aStream nextPutAll: 'className: ' , self className printString.
			hasPreLoadDoIt | hasPostLoadDoIt | hasLoads | hasProjectPackage ifTrue: [ aStream nextPut: $; ] ].
	hasPreLoadDoIt ifTrue: [
			hasClassName | hasProjectPackage | hasLoads | hasPreLoadDoIt
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
			hasPostLoadDoIt | hasProjectPackage | hasLoads ifTrue: [ aStream nextPut: $; ] ].
	hasPostLoadDoIt ifTrue: [
			hasClassName | hasProjectPackage | hasLoads | hasPostLoadDoIt
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
			hasProjectPackage | hasLoads ifTrue: [ aStream nextPut: $; ] ].

	hasLoads ifTrue: [
			hasClassName | hasProjectPackage | hasPreLoadDoIt | hasPostLoadDoIt
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
			| hasRepo |
			hasRepo := self hasRepository.
			hasRepo ifTrue: [
					| repos |
					repos := self repositories map values.
					repos size = 1
						ifTrue: [
								fromShortCut
									ifTrue: [
											hasClassName | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt
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

	| hasProjectPackage hasLoads hasClassName hasPreLoadDoIt hasPostLoadDoIt |
	hasClassName := self hasClassName.
	hasProjectPackage := self hasRepository or: [ hasClassName & (className ~= self name) ].
	hasLoads := self loads isNotNil.
	hasPreLoadDoIt := self preLoadDoIt isNotNil.
	hasPostLoadDoIt := self postLoadDoIt isNotNil.
	hasClassName | hasProjectPackage | hasLoads | hasPreLoadDoIt | hasPostLoadDoIt ifTrue: [
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

	self version ifNotNil: [ :vrsn | ^ vrsn spec currentlyLoadedClassesInVersion ].
	^ #(  )
]

{ #category : 'loading' }
MetacelloProjectSpec >> ensureProjectLoadedWithEngine: anEngine [
	"Ensure that the MetacelloProject version is built (We downloaded the baseline and we executed it to build a MetacelloVersion"

	(self hasVersion and: [ (self isMutable and: [ anEngine ignoreImage ]) not ]) ifTrue: [ ^ self ].

	project := anEngine loader loadProject: self
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
			  (self hasVersion and: [ aMetacelloProjectSpec hasVersion ]) and: [
				  (self repositories isEmpty or: [ aMetacelloProjectSpec repositories isEmpty ]) or: [
					  self repositories hasNoLoadConflicts: aMetacelloProjectSpec repositories ] ] ]
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasNonVersionStringField [

	| hasProjectPackage |
	self hasClassName ifTrue: [ ^ true ].

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

{ #category : 'testing' }
MetacelloProjectSpec >> hasVersion [

	^ self version isNotNil
]

{ #category : 'mutability' }
MetacelloProjectSpec >> immutable [
    mutable := false
]

{ #category : 'scripting' }
MetacelloProjectSpec >> isLocked [

  ^ self registration 
	ifNotNil: [ :aRegistration | aRegistration locked ]
	ifNil: [ false ]
]

{ #category : 'mutability' }
MetacelloProjectSpec >> isMutable [

	^ mutable ifNil: [ ^ true ]
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
	hash := String stringHash: self preLoadDoIt asString initialHash: hash.
	hash := String stringHash: self postLoadDoIt asString initialHash: hash.
	hash := hash bitXor: loads hash.
	^ hash bitXor: self repositories metacelloRegistrationHash
]

{ #category : 'mutability' }
MetacelloProjectSpec >> mutable [
    mutable := true
]

{ #category : 'querying' }
MetacelloProjectSpec >> name [

	^name
]

{ #category : 'accessing' }
MetacelloProjectSpec >> name: aString [

	self shouldBeMutable.
	name := aString trim.
	self resetProjectPackage
]

{ #category : 'merging' }
MetacelloProjectSpec >> nonOverridable [
    ^ super nonOverridable , #(#'projectPackage' #'repositories')
]

{ #category : 'copying' }
MetacelloProjectSpec >> postCopy [

	super postCopy.
	mutable := nil.
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

{ #category : 'accessing' }
MetacelloProjectSpec >> project [

	^ project
]

{ #category : 'accessing' }
MetacelloProjectSpec >> project: aMetacelloProject [

	project := aMetacelloProject
]

{ #category : 'visiting' }
MetacelloProjectSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	projectBlock value: self
]

{ #category : 'printing' }
MetacelloProjectSpec >> projectLabel [

	^ 'baseline'
]

{ #category : 'accessing' }
MetacelloProjectSpec >> projectPackage [
	"I return a MetacelloPackageSpec corresponding to the BaselineOf of the project."

	^ projectPackage ifNil: [
			  self className ifNil: [ ^ nil ].
			  projectPackage := self project packageSpec
				                    name: self className;
				                    repositories: self getRepositories;
				                    yourself ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> projectReference [
	
	^ self
]

{ #category : 'scripting' }
MetacelloProjectSpec >> registration [

	^ MetacelloProjectRegistration registrationForProjectSpec: self ifPresent: [ :existing | existing ]
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repositories [

	^ repositories ifNil: [ repositories := MetacelloRepositoriesSpec new ]
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repositories: anObject [
    self shouldBeMutable.
    repositories := anObject.
    self resetProjectPackage
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repository: aStringOrMetacelloRepositorySpec [
    self repositories repository: aStringOrMetacelloRepositorySpec.
    self resetProjectPackage
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repository: aString username: username password: password [

	self repositories repository: aString username: username password: password.
	self resetProjectPackage
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

	"must parallel implementation of MetacelloProject>>setBaselineRepositoryDescription: we want the same repoSpec"

	| spec repo |
	self repositorySpecs do: [ :repoSpec | spec := repoSpec ].
	[ repo := spec createRepository ]
		on: Error
		do: [ :ex | ^ '' ].
	^ repo repositoryVersionString
]

{ #category : 'private' }
MetacelloProjectSpec >> resetProjectPackage [

	projectPackage := nil
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

{ #category : 'mutability' }
MetacelloProjectSpec >> shouldBeMutable [

	self isMutable ifTrue: [ ^ self ].
	self error: 'Not allowed to modify an immutable object'
]

{ #category : 'scripting' }
MetacelloProjectSpec >> unregisterProject [

	^ MetacelloProjectRegistration registrationForProjectSpec: self ifPresent: [ :existing | existing unregisterProject ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> version [

	^ self project ifNotNil: [ :aProject | aProject version ]
]
