Class {
	#name : 'MetacelloRepositoryGroup',
	#superclass : 'Object',
	#instVars : [
		'repositories'
	],
	#category : 'Metacello-Core-Gofer',
	#package : 'Metacello-Core',
	#tag : 'Gofer'
}

{ #category : 'instance creation' }
MetacelloRepositoryGroup class >> onRepositories: aRepositoryCollection [

	^ self new
		repositories: aRepositoryCollection;
		yourself
]

{ #category : 'fetching' }
MetacelloRepositoryGroup >> fetchPackageNamed: aString [

	self uniqueRepository fetchPackageNamed: aString
]

{ #category : 'accessing' }
MetacelloRepositoryGroup >> loadPackageNamed: aName intoLoader: aLoader [
	"Find the first repository that defines the package"

	repositories do: [ :repo |
		repo
			packageNamed: aName
			ifPresent: [ :found |
			^ repo loadPackageNamed: aName intoLoader: aLoader ] ]
]

{ #category : 'accessing' }
MetacelloRepositoryGroup >> repositories: aCollection [ 
	repositories := aCollection
]

{ #category : 'accessing' }
MetacelloRepositoryGroup >> repositoryDescription [

	^ self uniqueRepository repositoryDescription
]

{ #category : 'accessing' }
MetacelloRepositoryGroup >> repositoryVersionString [

	^ self uniqueRepository repositoryVersionString
]

{ #category : 'accessing' }
MetacelloRepositoryGroup >> uniqueRepository [

	repositories asSet isEmpty ifTrue: [
		self error: 'No repositories found!' ].
	repositories asSet size = 1 ifTrue: [
		self error: 'Conflicting repositories!' ].
	^ repositories anyOne
]
