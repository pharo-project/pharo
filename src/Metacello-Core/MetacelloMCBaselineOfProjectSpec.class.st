Class {
	#name : 'MetacelloMCBaselineOfProjectSpec',
	#superclass : 'MetacelloProjectSpec',
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'visiting' }
MetacelloMCBaselineOfProjectSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitProjectSpec: self
]

{ #category : 'scripting' }
MetacelloMCBaselineOfProjectSpec >> asBaselineProjectSpec [
    ^ self
]

{ #category : 'scripting' }
MetacelloMCBaselineOfProjectSpec >> asProjectRegistration [
    ^ MetacelloProjectRegistration fromMCBaselineProjectSpec: self
]

{ #category : 'scripting' }
MetacelloMCBaselineOfProjectSpec >> canDowngradeTo: aProjectSpec [
    "cannot upgrade between baselines"

    ^ false
]

{ #category : 'scripting' }
MetacelloMCBaselineOfProjectSpec >> canUpgradeTo: aProjectSpec [
  "cannot upgrade between baselines unless repositories can be upgraded"

  (self repositories canUpgradeTo: aProjectSpec repositories)
    ifTrue: [ ^ true ].
  ^ false
]

{ #category : 'printing' }
MetacelloMCBaselineOfProjectSpec >> configHasVersionString [
  ^ false
]

{ #category : 'private' }
MetacelloMCBaselineOfProjectSpec >> constructClassName [
    ^ 'BaselineOf' , self name
]

{ #category : 'mutability' }
MetacelloMCBaselineOfProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [
  | copy |
  aMetacelloProjectRegistration
    baselineProjectSpecIfPresent: [ :spec | 
      copy := spec copy.
      aBlock value: copy.
      aMetacelloProjectRegistration baselineProjectSpec: copy ]
    ifAbsent: [ 
      aMetacelloProjectRegistration
        configurationProjectSpecIfPresent: [ :spec | 
          copy := spec copy.
          aBlock value: copy.
          aMetacelloProjectRegistration configurationProjectSpec: copy ]
        ifAbsent: [ aBlock value: nil ] ]
]

{ #category : 'loading' }
MetacelloMCBaselineOfProjectSpec >> determineCurrentVersionForLoad [
    ^ self version
]

{ #category : 'loading' }
MetacelloMCBaselineOfProjectSpec >> ensureConfigurationLoaded: vrsn ensured: ensured withEngine: anObject [
  "answer true if the configuration should be reloaded"

  "see Issue #181 for details ... basically we always want to consider loading the baseline from a project reference, especially if the two project specs are not the same..."

  "https://github.com/dalehenrich/metacello-work/issues/181"

  ^ true
]

{ #category : 'printing' }
MetacelloMCBaselineOfProjectSpec >> hasClassName [

	^ className isNotNil and: [ className ~= self constructClassName ]
]

{ #category : 'testing' }
MetacelloMCBaselineOfProjectSpec >> hasConflictWithConfigurationSpec: projectSpec [
  "baseline can be loaded on top of a configuration without a conflict"

  ^ false
]

{ #category : 'testing' }
MetacelloMCBaselineOfProjectSpec >> hasConflictWithProjectSpec: projectSpec [
	^ projectSpec hasConflictWithBaselineSpec: self
]

{ #category : 'testing' }
MetacelloMCBaselineOfProjectSpec >> isBaselineOfProjectSpec [
	^ true
]

{ #category : 'importing' }
MetacelloMCBaselineOfProjectSpec >> mergeImportLoads: aLoadList [
    aLoadList
        ifNotNil: [ :otherLoads | self loads ifNil: [ loads := otherLoads ] ifNotNil: [ loads := loads , otherLoads ] ]
]

{ #category : 'merging' }
MetacelloMCBaselineOfProjectSpec >> mergeRepositoriesSpec: anotherRepositories [
  "anotherRepositories wins ... there can ever only be one repository for the 
   baseline to load from"

  "https://github.com/dalehenrich/metacello-work/issues/251"

  self repositories: anotherRepositories
]

{ #category : 'merging' }
MetacelloMCBaselineOfProjectSpec >> mergeSpec: anotherSpec [
    ^ super mergeSpec: anotherSpec asBaselineProjectSpec
]

{ #category : 'printing' }
MetacelloMCBaselineOfProjectSpec >> projectLabel [
    ^ 'baseline'
]

{ #category : 'querying' }
MetacelloMCBaselineOfProjectSpec >> repositoryBranchName [
	"extract a branch name from the repository ... if possible"

	"must parallel implementation of MetacelloMCBaselineProject>>setBaselineRepositoryDescription: we want the same repoSpec"

	| spec |
	self repositorySpecs do: [ :repoSpec | spec := repoSpec ].
	^ spec createRepository repositoryBranchName
]

{ #category : 'querying' }
MetacelloMCBaselineOfProjectSpec >> repositoryVersionString [
  "extract a version string from the repository ... if possible"

  "must parallel implementation of MetacelloMCBaselineProject>>setBaselineRepositoryDescription: we want the same repoSpec"

  | spec repo |
  self repositorySpecs do: [ :repoSpec | spec := repoSpec ].
  [ repo := spec createRepository ]
    on: Error
    do: [ :ex | ^ '' ].
  ^ repo repositoryVersionString
]

{ #category : 'scripting' }
MetacelloMCBaselineOfProjectSpec >> validateVersionString: issues withDefaultVersionString: ignored [

	self versionString ifNotNil: [ :vs |
		| prj |
		prj := self project asBaselineProject.
		vs ~= prj singletonVersionName ifTrue: [
			issues add: (MetacelloValidationError
					 reasonCode: #invalidVersionString
					 callSite:
					 #validateForScriptLoad:withDefaultVersionString:withDefaultRepositoryDecription:
					 explanation: 'version field is incorrect, should be: '
						 , prj singletonVersionName printString) ] ]
]

{ #category : 'querying' }
MetacelloMCBaselineOfProjectSpec >> version [

	projectInitialized ifFalse: [ ^ nil ].
	^ self project version
]

{ #category : 'querying' }
MetacelloMCBaselineOfProjectSpec >> versionString [

	^ versionString ifNil: [
		  self version ifNotNil: [ :v | v versionString ] ]
]
