Extension { #name : #BaselineOf }

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> allPackageNames [
	
	^ self version packages collect: #name
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> deepPackagesOfGroupNamed: aName [
	"Traverses the group tree to collect packages"
	| allGroups group |
	
	allGroups := self version groups.
	group := allGroups detect: [ :eachGroup | eachGroup name = aName ].

	^ (group includes
		collect: [ :each | 
			(allGroups anySatisfy: [ :eachGroup | eachGroup name = each ])
				ifTrue: [ self deepPackagesOfGroupNamed: each ]
				ifFalse: [ { each } ] ])
		flattened
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf >> packageRepository [

	" Tries to determine a repository from which the baseline is being loaded. Useful for 
	refering other baselines in the same repository. "
	
	^ (self class package mcWorkingCopy repositoryGroup repositories reject: [:each | each = MCCacheRepository uniqueInstance]) 
		ifNotEmpty: [ :repositories | repositories anyOne ]
		ifEmpty: [ nil ]


	
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf >> packageRepositoryURL [

	" Tries to determine a repository URL from which the baseline is being loaded. Useful for 
	refering other baselines in the same repository. "

	^ self packageRepository
		ifNotNil: [ :aRepository | aRepository description ]
		ifNil: [ '' ]


	
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> packagesOfGroupNamed: aName [

	^ (self version groups detect: [ :g | g name = aName ]) includes
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> version [
	
	^ self project version
]
