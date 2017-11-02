Class {
	#name : #MetacelloRepositorySpec,
	#superclass : #MetacelloSpec,
	#instVars : [
		'description',
		'username',
		'password',
		'type'
	],
	#category : 'Metacello-MC-Specs'
}

{ #category : #adding }
MetacelloRepositorySpec >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec addMember 
			name: self name;
			spec: self;
			yourself)
]

{ #category : #'mc support' }
MetacelloRepositorySpec >> canUpgradeTo: aMetacelloRepositorySpec [
  self description = aMetacelloRepositorySpec description
    ifTrue: [ ^ true ].
  (#('github' 'gitorious' 'bitbucket') includes: self type)
    ifTrue: [ ^ self createRepository canUpgradeTo: aMetacelloRepositorySpec createRepository ].
  ^ false
]

{ #category : #printing }
MetacelloRepositorySpec >> configMethodCascadeOn: aStream lastCascade: lastCascade [

	aStream nextPutAll: 'repository: ', self description printString.
	(self username isEmpty not or: [ self password isEmpty not ])
		ifTrue: [ aStream nextPutAll: ' username: ', self username printString, ' password: ', self password printString ].
	lastCascade ifFalse: [ aStream nextPut: $;; cr ].
]

{ #category : #printing }
MetacelloRepositorySpec >> configMethodOn: aStream indent: indent [

	aStream 
		tab: indent; 
		nextPutAll: 'spec '.
	self configMethodCascadeOn: aStream lastCascade: true
]

{ #category : #'mc support' }
MetacelloRepositorySpec >> createRepository [
  | repo |
  repo := self project createRepository: self.
  ^ MCRepositoryGroup default repositories
    detect: [ :each | each = repo ]
    ifNone: [ 
      MCRepositoryGroup default addRepository: repo.
      repo ]
]

{ #category : #querying }
MetacelloRepositorySpec >> description [

	^description
]

{ #category : #accessing }
MetacelloRepositorySpec >> description: aString [

	description := aString
]

{ #category : #private }
MetacelloRepositorySpec >> extractTypeFromDescription [

	^MetacelloPlatform current extractTypeFromDescription: self description
]

{ #category : #'mc support' }
MetacelloRepositorySpec >> hasNoLoadConflicts: aMetacelloRepositorySpec [
  self description = aMetacelloRepositorySpec description
    ifTrue: [ ^ true ].
  self type = aMetacelloRepositorySpec type
    ifFalse: [ 
      "if the types are different then we don't need to create the repoitory to 
       know that there will be conflicts"
      ^ false ].
  (#('github' 'gitorious' 'bitbucket') includes: self type)
    ifTrue: [ 
      ^ self createRepository
        hasNoLoadConflicts: aMetacelloRepositorySpec createRepository ].
  ^ false
]

{ #category : #private }
MetacelloRepositorySpec >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec mergeMember 
			name: self name;
			spec: self;
			yourself)
]

{ #category : #merging }
MetacelloRepositorySpec >> mergeMap [
    | map |
    map := super mergeMap.
    map at: #'description' put: description.
    map at: #'type' put: self type.
    map at: #'username' put: username.
    map at: #'password' put: password.
    ^ map
]

{ #category : #querying }
MetacelloRepositorySpec >> name [

	^self description
]

{ #category : #querying }
MetacelloRepositorySpec >> password [

	password == nil ifTrue: [ password := '' ].
	^password
]

{ #category : #accessing }
MetacelloRepositorySpec >> password: aString [

	password := aString
]

{ #category : #private }
MetacelloRepositorySpec >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec removeMember 
			name: self name;
			spec: self;
			yourself)
]

{ #category : #querying }
MetacelloRepositorySpec >> type [

	type == nil ifTrue: [ type := self extractTypeFromDescription ].
	^type
]

{ #category : #accessing }
MetacelloRepositorySpec >> type: aString [

	type := aString
]

{ #category : #querying }
MetacelloRepositorySpec >> username [

	username == nil ifTrue: [ username := '' ].
	^username
]

{ #category : #accessing }
MetacelloRepositorySpec >> username: aString [

	username := aString
]
