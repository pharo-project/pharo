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

{ #category : 'accessing' }
MetacelloRepositoryGroup >> loadPackageNamed: aName intoLoader: aLoader [
	"Find the first repository that defines the package"

	repositories do: [ :repo |
		repo
			packageNamed: aName
			ifPresent: [ :found |
			^ repo loadPackageNamed: aName intoLoader: aLoader ] ]
]
