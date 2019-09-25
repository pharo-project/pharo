Extension { #name : #BaselineOf }

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> allPackageNames [
	
	^ self version packages collect: #name
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

	| url packageName |
	url := self packageRepository
		ifNotNil: [ :aRepository | aRepository description ]
		ifNil: [ '' ].
	url ifNotEmpty: [ ^ url ].
	
	self class environment at: #IceRepository ifPresent: [ :class |
		"The following is in case the url of the repository could not be determined. It looks at iceberg repositories in the image to see if any contains the baseline we are trying to load. If there is one, we return its repository url"
		packageName := self class package name.
		^ 'tonel://' , (class registry detect: [ :repo | repo workingCopy packageNames includes: packageName ] ifFound: [ :repo | repo location pathString , '/' , repo subdirectory ] ifNone: [ '' ])
	].


	
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> packagesOfGroupNamed: aName [

	^ (self version groups detect: [ :g | g name = aName ]) includes
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> version [
	
	^ self project version
]
