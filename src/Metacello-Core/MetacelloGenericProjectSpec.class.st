Class {
	#name : #MetacelloGenericProjectSpec,
	#superclass : #MetacelloProjectSpec,
	#instVars : [
		'projectPackage',
		'repositories'
	],
	#category : #Metacello-Core-Specs
}

{ #category : #scripting }
MetacelloGenericProjectSpec >> compareEqual: aMetacelloProjectSpec [
    "'projectPackage repositories'"

    ^ (super compareEqual: aMetacelloProjectSpec)
        and: [ self repositories compareEqual: aMetacelloProjectSpec repositories ]
]

{ #category : #loading }
MetacelloGenericProjectSpec >> determineCurrentVersionForLoad [
    self subclassResponsibility
]

{ #category : #accessing }
MetacelloGenericProjectSpec >> getRepositories [
    "raw access to iv"

    ^ repositories
]

{ #category : #scripting }
MetacelloGenericProjectSpec >> hasNoLoadConflicts: aMetacelloProjectSpec [
  "'projectPackage repositories'"

  ^ (super hasNoLoadConflicts: aMetacelloProjectSpec)
    and: [ 
      (self repositories isEmpty or: [ aMetacelloProjectSpec repositories isEmpty ])
        or: [ self repositories hasNoLoadConflicts: aMetacelloProjectSpec repositories ] ]
]

{ #category : #testing }
MetacelloGenericProjectSpec >> hasRepository [
    ^ self repositorySpecs notEmpty
]

{ #category : #loading }
MetacelloGenericProjectSpec >> load [
    (MetacelloLookupProjectSpecForLoad new
        projectSpec: self;
        yourself) signal performLoad
]

{ #category : #loading }
MetacelloGenericProjectSpec >> loadVersion: aVersionOrNil [
    self subclassResponsibility
]

{ #category : #merging }
MetacelloGenericProjectSpec >> mergeMap [
    | map |
    map := super mergeMap.
    map at: #'repositories' put: repositories.
    ^ map
]

{ #category : #merging }
MetacelloGenericProjectSpec >> mergeRepositoriesSpec: anotherRepositories [
  self
    repositories:
      (self getRepositories == nil
        ifTrue: [ anotherRepositories ]
        ifFalse: [ self repositories mergeSpec: anotherRepositories ])
]

{ #category : #scripting }
MetacelloGenericProjectSpec >> mergeScriptRepository: anotherSpec [
    self repositories: anotherSpec repositories
]

{ #category : #merging }
MetacelloGenericProjectSpec >> mergeSpec: anotherSpec [
  | newSpec map anotherRepositories |
  newSpec := super mergeSpec: anotherSpec.
  map := anotherSpec mergeMap.
  (anotherRepositories := map at: #'repositories') ~~ nil
    ifTrue: [ newSpec mergeRepositoriesSpec: anotherRepositories ].
  ^ newSpec
]

{ #category : #scripting }
MetacelloGenericProjectSpec >> metacelloRegistrationHash [
    "projectPackage (ignored) repositories"

    ^ super metacelloRegistrationHash bitXor: self repositories metacelloRegistrationHash
]

{ #category : #accessing }
MetacelloGenericProjectSpec >> name: aString [
    super name: aString.
    self projectPackage: nil
]

{ #category : #merging }
MetacelloGenericProjectSpec >> nonOverridable [
    ^ super nonOverridable , #(#'projectPackage' #'repositories')
]

{ #category : #copying }
MetacelloGenericProjectSpec >> postCopy [
    super postCopy.
    repositories := repositories copy.
    projectPackage := nil
]

{ #category : #accessing }
MetacelloGenericProjectSpec >> projectPackage [
    self subclassResponsibility
]

{ #category : #accessing }
MetacelloGenericProjectSpec >> projectPackage: aProjectPackage [
    self shouldBeMutable.
    projectPackage := aProjectPackage
]

{ #category : #construction }
MetacelloGenericProjectSpec >> projectPackage: aBlock constructor: aVersionConstructor [
    aVersionConstructor projectPackageForProject: aBlock
]

{ #category : #accessing }
MetacelloGenericProjectSpec >> repositories [
    repositories ifNil: [ repositories := self project repositoriesSpec ].
    ^ repositories
]

{ #category : #accessing }
MetacelloGenericProjectSpec >> repositories: anObject [
    self shouldBeMutable.
    repositories := anObject.
    self projectPackage: nil
]

{ #category : #construction }
MetacelloGenericProjectSpec >> repositories: aBlock constructor: aVersionConstructor [
    aVersionConstructor repositoriesForProject: aBlock
]

{ #category : #querying }
MetacelloGenericProjectSpec >> repository [
    | specs |
    self deprecated: 'Use repositories or repositorySpecs'.
    (specs := self repositorySpecs) isEmpty
        ifTrue: [ ^ nil ].
    ^ specs first
]

{ #category : #accessing }
MetacelloGenericProjectSpec >> repository: aStringOrMetacelloRepositorySpec [
    self repositories repository: aStringOrMetacelloRepositorySpec.
    self projectPackage: nil
]

{ #category : #construction }
MetacelloGenericProjectSpec >> repository: anObject constructor: aVersionConstructor [
    aVersionConstructor repositoryForProject: anObject
]

{ #category : #accessing }
MetacelloGenericProjectSpec >> repository: aString username: username password: password [
    self repositories repository: aString username: username password: password.
    self projectPackage: nil
]

{ #category : #construction }
MetacelloGenericProjectSpec >> repository: aString username: username password: password constructor: aVersionConstructor [
    aVersionConstructor repositoryForProject: aString username: username password: password
]

{ #category : #querying }
MetacelloGenericProjectSpec >> repositoryDescriptions [
    ^ self repositorySpecs collect: [ :repoSpec | repoSpec description ]
]

{ #category : #querying }
MetacelloGenericProjectSpec >> repositorySpecs [
    repositories ifNil: [ ^ #() ].
    ^ self repositories map values
]
