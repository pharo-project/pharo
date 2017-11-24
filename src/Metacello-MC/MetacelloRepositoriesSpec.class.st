Class {
	#name : #MetacelloRepositoriesSpec,
	#superclass : #MetacelloMemberListSpec,
	#category : #'Metacello-MC-Specs'
}

{ #category : #actions }
MetacelloRepositoriesSpec >> add: aStringOrSpec [

	aStringOrSpec addToMetacelloRepositories: self
]

{ #category : #scripting }
MetacelloRepositoriesSpec >> canUpgradeTo: aMetacelloRepositoriesSpec [
  | repositorySpecs anotherRepositorySpecs |
  repositorySpecs := self map values
    sort: [ :a :b | a description <= b description ].
  anotherRepositorySpecs := aMetacelloRepositoriesSpec map values
    sort: [ :a :b | a description <= b description ].
  repositorySpecs size ~= anotherRepositorySpecs size
    ifTrue: [ ^ false ].
  1 to: repositorySpecs size do: [ :index | 
    | repoSpec anotherRepoSpec |
    repoSpec := repositorySpecs at: index.
    anotherRepoSpec := anotherRepositorySpecs at: index.
    (repoSpec canUpgradeTo: anotherRepoSpec)
      ifTrue: [ ^ true ] ].
  ^ false
]

{ #category : #scripting }
MetacelloRepositoriesSpec >> compareEqual: aMetacelloProjectSpec [
    | repositorySpecs anotherRepositorySpecs |
    repositorySpecs := (self map values sort: [ :a :b | a description <= b description ])
        collect: [ :each | each description ].
    anotherRepositorySpecs := (aMetacelloProjectSpec map values sort: [ :a :b | a description <= b description ])
        collect: [ :each | each description ].
    ^ repositorySpecs = anotherRepositorySpecs
]

{ #category : #printing }
MetacelloRepositoriesSpec >> configMethodCascadeOn: aStream indent: indent [

	| repositorySpecs |
	repositorySpecs := self map values sort: [:a :b | a description <= b description ].
	repositorySpecs size = 1
		ifTrue: [ repositorySpecs first configMethodCascadeOn: aStream lastCascade: true ]
		ifFalse: [
			1 to: repositorySpecs size do: [:index | | repositorySpec |
				aStream tab: indent + 1.
				(repositorySpecs at: index)
					configMethodCascadeOn: aStream 
					lastCascade: index >= repositorySpecs size ]]
]

{ #category : #printing }
MetacelloRepositoriesSpec >> configMethodOn: aStream indent: indent [

	aStream 
		tab: indent; 
		nextPutAll: 'spec';
		cr.
	self configMethodCascadeOn: aStream indent: indent
]

{ #category : #scripting }
MetacelloRepositoriesSpec >> hasNoLoadConflicts: aMetacelloRepositoriesSpec [
  | repositorySpecs anotherRepositorySpecs |
  repositorySpecs := self map values
    sort: [ :a :b | a description <= b description ].
  anotherRepositorySpecs := aMetacelloRepositoriesSpec map values
    sort: [ :a :b | a description <= b description ].
  repositorySpecs size ~= anotherRepositorySpecs size
    ifTrue: [ ^ false ].
  1 to: repositorySpecs size do: [ :index | 
    | repoSpec anotherRepoSpec |
    repoSpec := repositorySpecs at: index.
    anotherRepoSpec := anotherRepositorySpecs at: index.
    (repoSpec hasNoLoadConflicts: anotherRepoSpec)
      ifFalse: [ ^ false ] ].
  ^ true
]

{ #category : #actions }
MetacelloRepositoriesSpec >> merge: aRepositorySpec [

	aRepositorySpec mergeIntoMetacelloRepositories: self
]

{ #category : #scripting }
MetacelloRepositoriesSpec >> metacelloRegistrationHash [
    ^ ((self map values sort: [ :a :b | a description <= b description ]) collect: [ :each | each description ]) hash
]

{ #category : #actions }
MetacelloRepositoriesSpec >> remove: aRepositorySpec [

	aRepositorySpec removeFromMetacelloRepositories: self
]

{ #category : #actions }
MetacelloRepositoriesSpec >> repository: aStringOrSpec [

	aStringOrSpec addToMetacelloRepositories: self
]

{ #category : #construction }
MetacelloRepositoriesSpec >> repository: anObject constructor: aVersionConstructor [
    aVersionConstructor repositoryForRepositories: anObject
]

{ #category : #actions }
MetacelloRepositoriesSpec >> repository: description username: username password: password [

	| spec |
	spec := 
		(self project repositorySpec)
			description: description;
			username: username;
			password: password;
			yourself.
	self addMember: 
		(self addMember 
			name: spec name;
			spec: spec;
			yourself)
]

{ #category : #construction }
MetacelloRepositoriesSpec >> repository: aString username: username password: password constructor: aVersionConstructor [
    aVersionConstructor repositoryForRepositories: aString username: username password: password
]
