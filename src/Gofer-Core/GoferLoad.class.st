"
Loading for specific packages using Gofer
"
Class {
	#name : 'GoferLoad',
	#superclass : 'GoferOperation',
	#category : 'Gofer-Core-Operations',
	#package : 'Gofer-Core',
	#tag : 'Operations'
}

{ #category : 'private' }
GoferLoad >> addResolved: aResolvedReference [
	| version |
	version := aResolvedReference version.
	(self versions includes: version)
		ifTrue: [ ^ self ].
	self versions addLast: version.
	version dependencies do: [ :dependency |
		self addResolved: ((GoferVersionReference
			name: dependency versionInfo name) resolveWith: self gofer) ]
]

{ #category : 'private' }
GoferLoad >> defaultModel [
	^ MCVersionLoader new
]

{ #category : 'running' }
GoferLoad >> execute [

	self model hasVersions ifTrue: [ self model load ].
	self updateRepositories
]

{ #category : 'initialization' }
GoferLoad >> initializeOn: aGofer [
	super initializeOn: aGofer.
	aGofer resolved
		do: [ :each | self addResolved: each ]
		displayingProgress: 'Loading Versions'
]

{ #category : 'private' }
GoferLoad >> updateRepositories [
	"This code makes sure that all packages have a repository assigned, including the dependencies."

	self versions do: [ :version |
		gofer repositories do: [ :repository |
			version workingCopy repositoryGroup
				addRepository: repository ] ]
]

{ #category : 'accessing' }
GoferLoad >> versions [
	^ model versions
]
