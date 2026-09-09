Class {
	#name : 'MetacelloRepositoriesSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'repositorySpecs'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloRepositoriesSpec >> add: aStringOrSpec [

	| spec |
	spec := aStringOrSpec isString
		        ifTrue: [ MetacelloRepositorySpec description: aStringOrSpec ]
		        ifFalse: [ aStringOrSpec ].

	self repositoryNamed: spec description ifPresent: [ :aSpec | repositorySpecs remove: aSpec ].
	self repositorySpecs add: spec
]

{ #category : 'scripting' }
MetacelloRepositoriesSpec >> canUpgradeTo: aMetacelloRepositoriesSpec [

	| specs anotherRepositorySpecs |
	specs := self repositorySpecs sorted: [ :a :b | a description <= b description ].
	anotherRepositorySpecs := aMetacelloRepositoriesSpec repositorySpecs sorted: [ :a :b | a description <= b description ].
	specs size = anotherRepositorySpecs size ifFalse: [ ^ false ].
	1 to: specs size do: [ :index |
			| repoSpec anotherRepoSpec |
			repoSpec := specs at: index.
			anotherRepoSpec := anotherRepositorySpecs at: index.
			(repoSpec canUpgradeTo: anotherRepoSpec) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'scripting' }
MetacelloRepositoriesSpec >> compareEqual: aMetacelloProjectSpec [

	| specs anotherRepositorySpecs |
	specs := (self repositorySpecs sorted: [ :a :b | a description <= b description ]) collect: [ :each | each description ].
	anotherRepositorySpecs := (aMetacelloProjectSpec repositorySpecs sorted: [ :a :b | a description <= b description ]) collect: [ :each | each description ].
	^ specs = anotherRepositorySpecs
]

{ #category : 'scripting' }
MetacelloRepositoriesSpec >> hasNoLoadConflicts: aMetacelloRepositoriesSpec [

	| specs anotherRepositorySpecs |
	specs := self repositorySpecs sorted: [ :a :b | a description <= b description ].
	anotherRepositorySpecs := aMetacelloRepositoriesSpec repositorySpecs sorted: [ :a :b | a description <= b description ].
	specs size ~= anotherRepositorySpecs size ifTrue: [ ^ false ].
	1 to: specs size do: [ :index |
			| repoSpec anotherRepoSpec |
			repoSpec := specs at: index.
			anotherRepoSpec := anotherRepositorySpecs at: index.
			(repoSpec hasNoLoadConflicts: anotherRepoSpec) ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'testing' }
MetacelloRepositoriesSpec >> hasRepository [

	^ self repositorySpecs isNotEmpty
]

{ #category : 'actions' }
MetacelloRepositoriesSpec >> merge: aSpec [

	self repositoryNamed: aSpec description
		ifPresent: [ :spec | spec mergeSpec: aSpec ]
		ifAbsent: [ repositorySpecs add: aSpec ]
]

{ #category : 'merging' }
MetacelloRepositoriesSpec >> mergeSpec: anotherSpec [

	super mergeSpec: anotherSpec.
	anotherSpec repositorySpecs do: [ :spec | self merge: spec ]
]

{ #category : 'copying' }
MetacelloRepositoriesSpec >> postCopy [

	super postCopy.
	repositorySpecs := repositorySpecs copy
]

{ #category : 'printing' }
MetacelloRepositoriesSpec >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: ' ['.
	self repositorySpecs
		ifEmpty: [ aStream nextPutAll: 'Empty' ]
		ifNotEmpty: [ :specs | specs do: [ :spec | aStream nextPutAll: spec description ] separatedBy: [ aStream nextPutAll: ', ' ] ].
	aStream nextPutAll: ']'
]

{ #category : 'actions' }
MetacelloRepositoriesSpec >> repository: aStringOrSpec [

	self add: aStringOrSpec
]

{ #category : 'actions' }
MetacelloRepositoriesSpec >> repository: description username: username password: password [

	self add: ((MetacelloRepositorySpec description: description)
			 username: username;
			 password: password;
			 yourself)
]

{ #category : 'accessing' }
MetacelloRepositoriesSpec >> repositoryNamed: aString ifAbsent: aBlock [

	^ self repositorySpecs
		  detect: [ :spec | spec description = aString ]
		  ifNone: aBlock
]

{ #category : 'accessing' }
MetacelloRepositoriesSpec >> repositoryNamed: aString ifPresent: aBlock [

	^ self repositorySpecs
		  detect: [ :spec | spec description = aString ]
		  ifFound: aBlock
]

{ #category : 'accessing' }
MetacelloRepositoriesSpec >> repositoryNamed: aString ifPresent: aBlock ifAbsent: anotherBlock [

	^ self repositorySpecs
		  detect: [ :spec | spec description = aString ]
		  ifFound: aBlock
		  ifNone: anotherBlock
]

{ #category : 'accessing' }
MetacelloRepositoriesSpec >> repositorySpecs [

	^ repositorySpecs ifNil: [ repositorySpecs := OrderedCollection new ]
]
