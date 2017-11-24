"
A singleton class, holds the list of repositories. Can look for a requested VersionInfo among its repositories.
"
Class {
	#name : #MCRepositoryGroup,
	#superclass : #Object,
	#instVars : [
		'repositories',
		'useCache'
	],
	#classInstVars : [
		'default'
	],
	#category : #'Monticello-Versioning'
}

{ #category : #accessing }
MCRepositoryGroup class >> default [
	^ default ifNil: [default := self new]
]

{ #category : #'instance creation' }
MCRepositoryGroup class >> withRepositories: aCollection [ 
	| group |
	group  := self new.
	aCollection do: [ :each | group addRepository: each ].
	^ group 
		
]

{ #category : #adding }
MCRepositoryGroup >> addBasicRepository: aRepository [
	((repositories includes: aRepository) 
	or: [ aRepository == MCCacheRepository uniqueInstance ])
		ifFalse: [
			repositories add: aRepository.
			self announceRepositoryAdded: aRepository.
			self class default addRepository: aRepository ].
	^ aRepository
]

{ #category : #adding }
MCRepositoryGroup >> addRepository: aRepository [
	aRepository addTo: self.
	^ aRepository
]

{ #category : #adding }
MCRepositoryGroup >> addRepositoryGroup: aRepositoryGroup [
	aRepositoryGroup repositoriesDo: [ :each | 
		each addTo: self ]
]

{ #category : #adding }
MCRepositoryGroup >> addTo: aRepository [
	^ aRepository addRepositoryGroup: self
]

{ #category : #private }
MCRepositoryGroup >> announceRepositoryAdded: aRepository [
	SystemAnnouncer uniqueInstance
		announce:
			(MCRepositoryAdded new
				repository: aRepository;
				yourself)
]

{ #category : #private }
MCRepositoryGroup >> announceRepositoryRemoved: aRepository [
	SystemAnnouncer uniqueInstance
		announce:
			(MCRepositoryRemoved new
				repository: aRepository;
				yourself)
]

{ #category : #accessing }
MCRepositoryGroup >> disableCache [
	useCache := false
]

{ #category : #testing }
MCRepositoryGroup >> includes: aRepository [
	^ self repositories includes: aRepository
]

{ #category : #interface }
MCRepositoryGroup >> includesVersionNamed: aString [
	" check for existing version name in parallel over all repositories "
	| results |

	results := Array new: self repositories size.
	
	self repositories doWithIndex: [:repository :index | 
		" fork of test for each repository "
		results 
			at: index 
			put: (repository includesVersionNamed: aString) ].

	" check if any repository included the given versionName already"
	^ results anySatisfy: [:result| result = true ]	
]

{ #category : #initialization }
MCRepositoryGroup >> initialize [
	super initialize.
	repositories := OrderedCollection new
]

{ #category : #accessing }
MCRepositoryGroup >> removeRepository: aRepository [
	repositories remove: aRepository ifAbsent: [ ^ self ].
	self announceRepositoryRemoved: aRepository
]

{ #category : #accessing }
MCRepositoryGroup >> repositories [
	^ (self useCache 
		ifTrue: [Array with: MCCacheRepository uniqueInstance] 
		ifFalse: [Array new]) , repositories select: #isValid
]

{ #category : #enumerating }
MCRepositoryGroup >> repositoriesDo: aBlock [
	self repositories do: [:ea | [aBlock value: ea] on: Error do: []]
]

{ #category : #accessing }
MCRepositoryGroup >> useCache [
	^ useCache ifNil: [
		useCache := true
	]
]

{ #category : #interface }
MCRepositoryGroup >> versionWithInfo: aVersionInfo [
	^self versionWithInfo: aVersionInfo ifNone: [ self error: 'Could not find version ', aVersionInfo name printString,'. Maybe you need to add a repository?' ]
]

{ #category : #accessing }
MCRepositoryGroup >> versionWithInfo: aVersionInfo ifNone: aBlock [
	self repositoriesDo: [:ea | (ea versionWithInfo: aVersionInfo) ifNotNil: [:v | ^ v]].
	^aBlock value
]
