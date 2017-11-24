"
I encapsulate the context just before a commit.
"
Class {
	#name : #KomitStagingArea,
	#superclass : #Object,
	#instVars : [
		'packages',
		'filterBlock'
	],
	#classInstVars : [
		'current'
	],
	#category : #'Komitter-Models'
}

{ #category : #'instance creation' }
KomitStagingArea class >> current [

	^ self currentFilteredBy: [ true ]
]

{ #category : #'instance creation' }
KomitStagingArea class >> currentFilteredBy: aFilterBlock [

	^ super new
		filterBlock: aFilterBlock;
		yourself
]

{ #category : #'instance creation' }
KomitStagingArea class >> new [

	self shouldNotImplement
]

{ #category : #release }
KomitStagingArea class >> resetCurrent [

	current := nil
]

{ #category : #'adding/removing' }
KomitStagingArea >> addPackage: aPackage [

	(packages includes: aPackage)
		ifFalse: [ packages add: aPackage ]
]

{ #category : #'adding/removing' }
KomitStagingArea >> filterBlock [

	^ filterBlock
]

{ #category : #'adding/removing' }
KomitStagingArea >> filterBlock: aFilterBlock [

	filterBlock := aFilterBlock
]

{ #category : #accessing }
KomitStagingArea >> flush [

	packages := nil
]

{ #category : #accessing }
KomitStagingArea >> initializePackages [ 

	packages := MCWorkingCopy allManagers
		select: [ :workingCopy | workingCopy needsSaving and: (filterBlock cull: workingCopy) ]
		thenCollect: [ :workingCopy | 
			(KomitPackage package: workingCopy package)
				yourself ].
]

{ #category : #protocol }
KomitStagingArea >> isCurrent [

	^ self == self class current
]

{ #category : #accessing }
KomitStagingArea >> packages [

	packages ifNil: [ self initializePackages ].
	^ packages copy
]

{ #category : #accessing }
KomitStagingArea >> packages: aCollection [

	packages := aCollection asOrderedCollection
]

{ #category : #protocol }
KomitStagingArea >> remotes [
	| result |
	
	self packages ifEmpty: [ ^ #() ].
	self packages ifEmpty: [ ^ #() ].
	result := self packages first remotes.
	self packages allButFirst do: [ :each |
		result := result intersection: each remotes ].
	
	^ result collect: [ :each | each koRemote ]
]

{ #category : #protocol }
KomitStagingArea >> remotesFor: aCollection [
	| result |
	
	result := aCollection first remotes.
	aCollection allButFirst do: [ :each |
		result := result intersection: each remotes ].
	
	^ result collect: [ :each | each koRemote ]
]

{ #category : #'adding/removing' }
KomitStagingArea >> removePackage: aKomitPackage [

	self packages 	
		remove: aKomitPackage
]
