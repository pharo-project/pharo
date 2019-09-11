Class {
	#name : #MetacelloProject,
	#superclass : #Object,
	#instVars : [
		'versionMap',
		'symbolicVersionMap',
		'errorMap',
		'loader',
		'loaderClass',
		'loadType',
		'configuration',
		'projectAttributes',
		'versionNumberClass'
	],
	#category : #'Metacello-Core-Model'
}

{ #category : #'instance creation' }
MetacelloProject class >> new [

	| inst |
	inst := self basicNew.
	^inst configuration: inst; yourself
]

{ #category : #'instance creation' }
MetacelloProject class >> on: aConfig [

	^self basicNew configuration: aConfig; yourself
]

{ #category : #accessing }
MetacelloProject class >> versionConstructorClass [
    ^ MetacelloVersionConstructor
]

{ #category : #private }
MetacelloProject >> attributes [

	^(OrderedCollection with: #common)
		addAll: self platformAttributes;
		yourself
]

{ #category : #'spec classes' }
MetacelloProject >> baselineOfProjectSpec [
    ^ self baselineOfProjectSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloProject >> baselineOfProjectSpecClass [
    ^ self subclassResponsibility
]

{ #category : #'spec classes' }
MetacelloProject >> baselineOfVersionSpecClass [
    ^ self subclassResponsibility
]

{ #category : #versions }
MetacelloProject >> bleedingEdge [

	^self version: #bleedingEdge
]

{ #category : #accessing }
MetacelloProject >> configuration [
	^ configuration
]

{ #category : #accessing }
MetacelloProject >> configuration: anObject [
	configuration := anObject
]

{ #category : #'spec classes' }
MetacelloProject >> configurationOfProjectSpec [
    ^ self configurationOfProjectSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloProject >> configurationOfProjectSpecClass [
    ^ self subclassResponsibility
]

{ #category : #versions }
MetacelloProject >> currentVersion [
	| cacheKey cv |
	cacheKey := self configuration class.
	^ MetacelloPlatform current
		stackCacheFor: #currentVersion
		at: cacheKey
		doing: [ :cache | 
			cv := self currentVersionAgainst: nil.
			^ cache at: cacheKey put: cv ]
]

{ #category : #versions }
MetacelloProject >> currentVersionAgainst: resolvedPackageAndProjectNames [

	| cacheKey |

	cacheKey := resolvedPackageAndProjectNames
		ifNil: [ Array with: self configuration class with: nil ]
		ifNotNil: [ Array
				with: self configuration class
				with: ( resolvedPackageAndProjectNames sort: [ :a :b | a <= b ] )
			].
	^ MetacelloPlatform current
		stackCacheFor: #currentVersionAgainst:
		at: cacheKey
		doing: [ :cache | 
			| cv versions latestSomethingLoaded |

			cv := nil.
			versions := self sortedAndFilteredVersions.
			versions
				do: [ :version | 
					| status matchBlock |

					status := resolvedPackageAndProjectNames
						ifNil: [ version spec isPartiallyCurrent ]
						ifNotNil: [ version spec isPartiallyCurrentAgainst: resolvedPackageAndProjectNames ].
					matchBlock := [ :matchStatus | 
					cv := version copy.
					cv versionStatus: matchStatus.
					^ cache at: cacheKey put: cv
					].
					status isAllLoadedToSpec: matchBlock.
					status isLoadedToSpec: matchBlock.
					status isLoadedMatchConstraints: matchBlock.
					status
						isSomethingLoaded: [ :matchStatus | 
							latestSomethingLoaded
								ifNil: [ cv := version copy.
									cv versionStatus: matchStatus.
									latestSomethingLoaded := cv
									]
							]
					].
			latestSomethingLoaded ifNotNil: [ ^ cache at: cacheKey put: latestSomethingLoaded ].
			^ cache at: cacheKey put: nil
			]
]

{ #category : #accessing }
MetacelloProject >> defaultBlessing [

	^#release
]

{ #category : #'spec classes' }
MetacelloProject >> defaultLoaderClass [

	^MetacelloSpecLoader
]

{ #category : #private }
MetacelloProject >> defaultPlatformAttributes [

	^ MetacelloPlatform current defaultPlatformAttributes
]

{ #category : #versions }
MetacelloProject >> development [

	^self version: #development
]

{ #category : #accessing }
MetacelloProject >> errorMap [
	^ errorMap
]

{ #category : #accessing }
MetacelloProject >> errorMap: anObject [
    errorMap ifNil: [ errorMap := Dictionary new ].
    errorMap := anObject
]

{ #category : #private }
MetacelloProject >> excludeFromLatestVersion [

	^#(structural development broken baseline)
]

{ #category : #'spec classes' }
MetacelloProject >> groupSpec [

	^self groupSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloProject >> groupSpecClass [

	^MetacelloGroupSpec
]

{ #category : #versions }
MetacelloProject >> hasVersion: versionString [

	self version: versionString ifAbsent: [ ^false ].
	^true
]

{ #category : #accessing }
MetacelloProject >> label [

	^self configuration class name
]

{ #category : #versions }
MetacelloProject >> lastVersion [

	| coll |
	coll := (self map values asArray sort: [:a :b | a <= b ]) asOrderedCollection.
	coll isEmpty ifTrue: [ ^nil ].
	^coll last
]

{ #category : #versions }
MetacelloProject >> latestVersion [

	| excluded |
"	self deprecated: 'Please use #stableVersion instead.'.
"	self flag: 'deprecate after version 1.0'.
	excluded := self excludeFromLatestVersion.
	^(self map values select: [:version |  
		(excluded includes: version blessing) not ]) detectMax: [:version | version ]
]

{ #category : #versions }
MetacelloProject >> latestVersion: blessing [

	^(self map values select: [:version | blessing = version blessing ]) detectMax: [:version | version ]
]

{ #category : #versions }
MetacelloProject >> latestVersionMatching: versionPatternString [
	"Answer whether the version number of the receiver matches the given pattern string.

	 A Metacello version number is made up of version sequences delimited by the characters $. and $-.
	 The $. introduces a numeric version sequence and $- introduces an alphanumeric version sequence.
	 
	 A version pattern is made up of version pattern match sequences. also delimited by the characters $. 
	 and $-.. Each pattern match sequence is tested against the corresponding version sequence of the 
	 receiver, using the 'standard' pattern matching rules. All sequences must answer true for a match.
	
	 The special pattern sequence '?' is a match for the corresponding version sequence and all subsequent 
	 version sequences. '?' as the version pattern matches all versions. No more version pattern 
	 sequences are permitted once the '?' sequence is used. If used, it is the last version pattern
	 sequence."

	^self 
		latestVersionMatching: versionPatternString 
		includedBlessings: #()
		excludedBlessings: self excludeFromLatestVersion
]

{ #category : #versions }
MetacelloProject >> latestVersionMatching: versionPatternString excludedBlessings: excluded [

	^self 
		latestVersionMatching: versionPatternString 
		includedBlessings: #()
		excludedBlessings: excluded
]

{ #category : #versions }
MetacelloProject >> latestVersionMatching: versionPatternString includedBlessings: included [

	^self 
		latestVersionMatching: versionPatternString 
		includedBlessings: included
		excludedBlessings: self excludeFromLatestVersion
]

{ #category : #versions }
MetacelloProject >> latestVersionMatching: versionPatternString includedBlessings: included excludedBlessings: excludedBlessings [

	| excluded |
	excluded := excludedBlessings asSet copy.
	excluded removeAllFoundIn: included.
	^(self map values select: [:version |
		(included isEmpty or: [ included includes: version blessing ]) 
			and: [ (excluded includes: version blessing) not 
				and: [ version versionNumber match: versionPatternString ]]])
		detectMax: [:version | version ]
]

{ #category : #loading }
MetacelloProject >> load: aVersionString [

	^(self version: aVersionString) load
]

{ #category : #accessing }
MetacelloProject >> loadType [
	"#atomic or #linear"

	loadType == nil ifTrue: [ ^#atomic ].
	^loadType
]

{ #category : #accessing }
MetacelloProject >> loadType: aSymbol [
	"#atomic or #linear"

	loadType := aSymbol
]

{ #category : #'spec classes' }
MetacelloProject >> loader [

	^loader
]

{ #category : #'spec classes' }
MetacelloProject >> loader: aLoader [

	loader := aLoader
]

{ #category : #'spec classes' }
MetacelloProject >> loaderClass [

	loaderClass == nil ifTrue: [ loaderClass := self defaultLoaderClass ].
	^loaderClass
]

{ #category : #'spec classes' }
MetacelloProject >> loaderClass: aMetacelloSpecLoader [

	loaderClass := aMetacelloSpecLoader
]

{ #category : #private }
MetacelloProject >> lookupVersion: aVersionString ifAbsent: aBlock [
    "please use version:...this is a private method"

    ^ self map
        at: aVersionString
        ifAbsent: [ 
            (MetacelloVersionDefinitionError project: self project versionString: aVersionString)
                exception: (self errorMap at: aVersionString ifAbsent: [ ^ aBlock value ]);
                signal ]
]

{ #category : #accessing }
MetacelloProject >> map [
	versionMap ifNil: [ ^ Dictionary new ].
	^ versionMap
]

{ #category : #accessing }
MetacelloProject >> map: aDictionary [

	versionMap := aDictionary
]

{ #category : #'spec classes' }
MetacelloProject >> packagesSpec [

	^self packagesSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloProject >> packagesSpecClass [

	^MetacelloPackagesSpec
]

{ #category : #private }
MetacelloProject >> platformAttributes [

	 ^self projectPlatformAttributes
]

{ #category : #private }
MetacelloProject >> pragmaKeywords [

	^#(version:attribute: blessing:attribute: description:attribute: required:attribute: groups:attribute: doits:attribute:)
]

{ #category : #printing }
MetacelloProject >> printOn: aStream [
	| label |
	self configuration class printOn: aStream.
	aStream nextPut: $(.
	self versions
		do: [ :vrsn | 
			aStream nextPutAll: vrsn versionString.
			vrsn spec ~~ nil
				ifTrue: [ 
					(label := vrsn spec projectLabel) isEmpty
						ifFalse: [ aStream nextPutAll: ' [' , label , ']' ] ].
			aStream
				nextPut: $,;
				space ].
	aStream nextPut: $)
]

{ #category : #accessing }
MetacelloProject >> project [

	^self
]

{ #category : #private }
MetacelloProject >> projectAttributes [

	projectAttributes ~~ nil ifTrue: [ ^projectAttributes ].
	^#()
]

{ #category : #private }
MetacelloProject >> projectAttributes: aList [

	projectAttributes := aList
]

{ #category : #scripting }
MetacelloProject >> projectForScriptEngine: aMetacelloScriptEngine [
    ^ self projectForScriptEngine: aMetacelloScriptEngine unconditionalLoad: false
]

{ #category : #scripting }
MetacelloProject >> projectForScriptEngine: aMetacelloScriptEngine unconditionalLoad: aBool [
    ^ self subclassResponsibility
]

{ #category : #private }
MetacelloProject >> projectPlatformAttributes [

	| list aBlock |
	list := OrderedCollection new.
	(aBlock := self projectAttributes) ~~ nil 
		ifTrue: [ list addAll: aBlock value ].
	^self defaultPlatformAttributes, list
]

{ #category : #'spec classes' }
MetacelloProject >> projectReferenceSpec [

	^self projectReferenceSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloProject >> projectReferenceSpecClass [

	^MetacelloProjectReferenceSpec
]

{ #category : #'spec classes' }
MetacelloProject >> projectSpec [

	^self projectSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloProject >> projectSpecClass [
    ^ MetacelloProjectSpec
]

{ #category : #private }
MetacelloProject >> sortedAndFilteredVersions [

		^(self map values asArray sort: [:a :b | a >= b ]) select: [:vrsn | (#(structural broken baseline) includes: vrsn blessing) not ].
]

{ #category : #versions }
MetacelloProject >> stableVersion [

	^self version: #stable
]

{ #category : #accessing }
MetacelloProject >> symbolicVersionMap [

	^symbolicVersionMap
]

{ #category : #accessing }
MetacelloProject >> symbolicVersionMap: aDictionary [

	symbolicVersionMap := aDictionary
]

{ #category : #versions }
MetacelloProject >> symbolicVersionSymbols [

	^self symbolicVersionMap keys asArray sort: [:a :b | a <= b ]
]

{ #category : #'spec classes' }
MetacelloProject >> valueHolderSpec [

	^self valueHolderSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloProject >> valueHolderSpecClass [

	^MetacelloValueHolderSpec
]

{ #category : #versions }
MetacelloProject >> version: aVersionString [
    
    aVersionString isSymbol
        ifTrue: [ 
            | symbolicVersionString |
            symbolicVersionString := self symbolicVersionMap
                at: aVersionString
                ifAbsent: [ (MetacelloSymbolicVersionDoesNotExistError project: self project versionString: aVersionString) signal ].
            symbolicVersionString == #'notDefined'
                ifTrue: [ (MetacelloSymbolicVersionNotDefinedError project: self project versionString: aVersionString) signal ].
            ^ self
                lookupVersion: symbolicVersionString
                ifAbsent: [ (MetacelloSymbolicVersionDoesNotExistError project: self project versionString: symbolicVersionString) signal ] ].
    ^ self
        lookupVersion: aVersionString
        ifAbsent: [ (MetacelloVersionDoesNotExistError project: self project versionString: aVersionString) signal ]

]

{ #category : #versions }
MetacelloProject >> version: aVersionString ifAbsent: aBlock [

	^[ self version: aVersionString ] on: MetacelloVersionDoesNotExistError do: [:ex | aBlock value ].
]

{ #category : #versions }
MetacelloProject >> versionNumberClass [
    versionNumberClass ifNil: [ versionNumberClass := MetacelloVersionNumber ].
    ^ versionNumberClass
]

{ #category : #versions }
MetacelloProject >> versionNumberClass: aClass [
    versionNumberClass := aClass
]

{ #category : #'spec classes' }
MetacelloProject >> versionSpec [

	^self versionSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloProject >> versionSpecClass [

	^MetacelloVersionSpec
]

{ #category : #versions }
MetacelloProject >> versions [

	^self map values asArray sort: [:a :b | a <= b ]
]
