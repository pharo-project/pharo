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
		'strict'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
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

{ #category : 'scripting' }
MetacelloProjectSpec >> asProjectRegistration [

	^ MetacelloProjectRegistration projectSpec: self
]

{ #category : 'accessing' }
MetacelloProjectSpec >> baseName [

	^ self className withoutPrefix: 'BaselineOf'
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

{ #category : 'private' }
MetacelloProjectSpec >> constructClassName [

	^ 'BaselineOf' , self name
]

{ #category : 'accessing' }
MetacelloProjectSpec >> currentlyLoadedClassesInVersion [

	self version ifNotNil: [ :vrsn | ^ vrsn currentlyLoadedClassesInVersion ].
	^ #(  )
]

{ #category : 'loading' }
MetacelloProjectSpec >> ensureProjectLoadedWithEngine: anEngine [
	"Ensure that the MetacelloProject version is built (We downloaded the baseline and we executed it to build a MetacelloVersion"

	(self hasVersion and: [ anEngine ignoreImage not ]) ifTrue: [ ^ self ].

	project := anEngine loader loadProject: self engine: anEngine
]

{ #category : 'accessing' }
MetacelloProjectSpec >> getRepositories [
    "raw access to iv"

    ^ repositories
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
					  (self repositories hasRepository not or: [ aMetacelloProjectSpec repositories hasRepository not ]) or: [
						  self repositories hasNoLoadConflicts: aMetacelloProjectSpec repositories ] ] ]
]

{ #category : 'testing' }
MetacelloProjectSpec >> hasVersion [

	^ self version isNotNil
]

{ #category : 'scripting' }
MetacelloProjectSpec >> isLocked [

	^ MetacelloProjectRegistry registrationNamed: self className ifPresent: [ :existing | existing locked ] ifAbsent: [ false ]
]

{ #category : 'printing' }
MetacelloProjectSpec >> label [

	^self name
]

{ #category : 'private' }
MetacelloProjectSpec >> loadListForVersion: vrsn [

	^ (self loads isNil or: [ self loads isEmpty ])
		  ifTrue: [ vrsn defaultPackageNames ]
		  ifFalse: [ self loads ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> loads [
	^ loads
]

{ #category : 'accessing' }
MetacelloProjectSpec >> loads: aCollection [

	loads := aCollection isString
		         ifTrue: [ { aCollection } ]
		         ifFalse: [ aCollection asArray ]
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
	^ map
]

{ #category : 'scripting' }
MetacelloProjectSpec >> mergeScriptLoads: aSpec [

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
	"anotherRepositories wins ... there can ever only be one repository for the baseline to load from
		
		https://github.com/dalehenrich/metacello-work/issues/251"

	super mergeSpec: anotherSpec.
	anotherSpec getRepositories ifNotNil: [ :anotherRepositories | self repositories: anotherRepositories ]
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
	^ hash bitXor: (self repositories repositorySpecs collect: [ :each | each description ]) hash
]

{ #category : 'querying' }
MetacelloProjectSpec >> name [

	^name
]

{ #category : 'accessing' }
MetacelloProjectSpec >> name: aString [

	name := aString trim.
	self resetProjectPackage
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

	self validateDoItSelector: aSymbol.
	postLoadDoIt := aSymbol
]

{ #category : 'querying' }
MetacelloProjectSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloProjectSpec >> preLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	preLoadDoIt := aSymbol
]

{ #category : 'printing' }
MetacelloProjectSpec >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' [';
		nextPutAll: self baseName;
		nextPutAll: ']'
]

{ #category : 'accessing' }
MetacelloProjectSpec >> project [

	^ project
]

{ #category : 'accessing' }
MetacelloProjectSpec >> project: aMetacelloProject [

	project := aMetacelloProject
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
			  projectPackage := MetacelloPackageSpec new
				                    name: self className;
				                    repositories: self getRepositories;
				                    yourself ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> projectReference [
	
	^ self
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repositories [

	^ repositories ifNil: [ repositories := MetacelloRepositoriesSpec new ]
]

{ #category : 'accessing' }
MetacelloProjectSpec >> repositories: anObject [

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

	^ self repositories repositorySpecs
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

{ #category : 'accessing' }
MetacelloProjectSpec >> strict [
	
	^ strict
]

{ #category : 'accessing' }
MetacelloProjectSpec >> strict: aBoolean [
	
	strict := aBoolean
]

{ #category : 'scripting' }
MetacelloProjectSpec >> unregisterProject [

	^ MetacelloProjectRegistry registrationNamed: self className ifPresent: [ :existing | existing unregisterProject ]
]

{ #category : 'querying' }
MetacelloProjectSpec >> version [

	^ self project ifNotNil: [ :aProject | aProject version ]
]
