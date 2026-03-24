Class {
	#name : 'MetacelloRepositorySpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'description',
		'username',
		'password',
		'type',
		'resolvedReference'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
MetacelloRepositorySpec class >> description: aString [

	^ self new
		  description: aString;
		  yourself
]

{ #category : 'visiting' }
MetacelloRepositorySpec >> acceptVisitor: aVisitor [

	self error: 'Should not be visitable..'
]

{ #category : 'mc support' }
MetacelloRepositorySpec >> canUpgradeTo: aMetacelloRepositorySpec [
  self description = aMetacelloRepositorySpec description
    ifTrue: [ ^ true ].
  (#('github' 'gitorious' 'bitbucket') includes: self type)
    ifTrue: [ ^ self createRepository canUpgradeTo: aMetacelloRepositorySpec createRepository ].
  ^ false
]

{ #category : 'mc support' }
MetacelloRepositorySpec >> createRepository [

	| repo group startTime |
	
	startTime := Time millisecondClockValue.
	
	repo := MetacelloPlatform current createRepository: self.
	group := MCRepositoryGroup default.
	
	"If it is a new repository we log the time to create it"

	(group includes: repo) 
		ifFalse: [ MetacelloNotification 
			signal: 'Repository created for ' , self description 
			milliSeconds: (Time millisecondClockValue - startTime)].
	
	group addRepository: repo.
	^repo
]

{ #category : 'querying' }
MetacelloRepositorySpec >> description [

	^description
]

{ #category : 'accessing' }
MetacelloRepositorySpec >> description: aString [

	description := aString
]

{ #category : 'printing' }
MetacelloRepositorySpec >> fullDescription [

	^ resolvedReference ifNil: [ 'unresolved' ]
]

{ #category : 'mc support' }
MetacelloRepositorySpec >> hasNoLoadConflicts: aMetacelloRepositorySpec [

	self description = aMetacelloRepositorySpec description ifTrue: [ ^ true ].
	self type = aMetacelloRepositorySpec type ifFalse: [ "if the types are different then we don't need to create the repoitory to 
       know that there will be conflicts"
		^ false ].
	(#( 'github' 'gitorious' 'bitbucket' ) includes: self type) ifTrue: [ ^ self createRepository hasNoLoadConflicts: aMetacelloRepositorySpec createRepository ].
	^ false
]

{ #category : 'merging' }
MetacelloRepositorySpec >> mergeMap [

	| map |
	map := super mergeMap.
	map at: #description put: description.
	map at: #type put: self type.
	map at: #username put: username.
	map at: #password put: password.
	^ map
]

{ #category : 'querying' }
MetacelloRepositorySpec >> password [

	^ password ifNil: [ password := '' ]
]

{ #category : 'accessing' }
MetacelloRepositorySpec >> password: aString [

	password := aString
]

{ #category : 'printing' }
MetacelloRepositorySpec >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: ' ['.
	self description ifEmpty: [ aStream nextPutAll: 'No description' ] ifNotEmpty: [ :string | aStream nextPutAll: string ].
	aStream nextPutAll: ']'
]

{ #category : 'accessing' }
MetacelloRepositorySpec >> resolvedReference: aString [ 
	
	resolvedReference := aString
]

{ #category : 'querying' }
MetacelloRepositorySpec >> type [

	^ type ifNil: [ type := MetacelloPlatform current extractTypeFromDescription: self description ]
]

{ #category : 'accessing' }
MetacelloRepositorySpec >> type: aString [

	type := aString
]

{ #category : 'querying' }
MetacelloRepositorySpec >> username [

	^ username ifNil: [ username := '' ]
]

{ #category : 'accessing' }
MetacelloRepositorySpec >> username: aString [

	username := aString
]
