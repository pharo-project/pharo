Class {
	#name : 'MetacelloProject',
	#superclass : 'Object',
	#instVars : [
		'versionNumberClass',
		'projectAttributes',
		'errorMap',
		'symbolicVersionMap',
		'loadType',
		'versionMap',
		'configuration'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
MetacelloProject class >> new [

	| inst |
	inst := self basicNew.
	^inst configuration: inst; yourself
]

{ #category : 'instance creation' }
MetacelloProject class >> on: aConfig [

	^self basicNew configuration: aConfig; yourself
]

{ #category : 'accessing' }
MetacelloProject class >> versionConstructorClass [
    ^ MetacelloVersionConstructor
]

{ #category : 'scripting' }
MetacelloProject >> asBaselineProject [
    ^ MetacelloMCBaselineProject new
]

{ #category : 'scripting' }
MetacelloProject >> asConfigurationProject [
    ^ self
]

{ #category : 'private' }
MetacelloProject >> attributes [

	^ (OrderedCollection with: #common)
		  addAll: self platformAttributes;
		  yourself
]

{ #category : 'spec classes' }
MetacelloProject >> baselineOfProjectSpec [
    ^ self baselineOfProjectSpecClass for: self
]

{ #category : 'spec classes' }
MetacelloProject >> baselineOfProjectSpecClass [
    ^ MetacelloMCBaselineOfProjectSpec
]

{ #category : 'versions' }
MetacelloProject >> bleedingEdge [

	^self version: #bleedingEdge
]

{ #category : 'accessing' }
MetacelloProject >> configuration [
	^ configuration
]

{ #category : 'accessing' }
MetacelloProject >> configuration: anObject [
	configuration := anObject
]

{ #category : 'spec classes' }
MetacelloProject >> configurationOfProjectSpec [
    ^ MetacelloMCConfigurationOfProjectSpec for: self
]

{ #category : 'accessing' }
MetacelloProject >> defaultBlessing [

	^#release
]

{ #category : 'private' }
MetacelloProject >> defaultPlatformAttributes [

	^ Smalltalk image metacelloPlatformAttributes
]

{ #category : 'versions' }
MetacelloProject >> development [

	^self version: #development
]

{ #category : 'accessing' }
MetacelloProject >> errorMap [
	^ errorMap
]

{ #category : 'accessing' }
MetacelloProject >> errorMap: anObject [
    errorMap ifNil: [ errorMap := Dictionary new ].
    errorMap := anObject
]

{ #category : 'private' }
MetacelloProject >> excludeFromLatestVersion [

	^#(structural development broken baseline)
]

{ #category : 'spec classes' }
MetacelloProject >> groupSpec [

	^ MetacelloGroupSpec for: self
]

{ #category : 'versions' }
MetacelloProject >> hasVersion: versionString [

	self version: versionString ifAbsent: [ ^false ].
	^true
]

{ #category : 'accessing' }
MetacelloProject >> label [

	^self configuration class name
]

{ #category : 'versions' }
MetacelloProject >> lastVersion [

	| coll |
	coll := (self map values asArray sort: [:a :b | a <= b ]) asOrderedCollection.
	coll isEmpty ifTrue: [ ^nil ].
	^coll last
]

{ #category : 'versions' }
MetacelloProject >> latestVersion [

	| excluded |
"	self deprecated: 'Please use #stableVersion instead.'.
"	self flag: 'deprecate after version 1.0'.
	excluded := self excludeFromLatestVersion.
	^(self map values select: [:version |  
		(excluded includes: version blessing) not ]) detectMax: [:version | version ]
]

{ #category : 'versions' }
MetacelloProject >> latestVersion: blessing [

	^(self map values select: [:version | blessing = version blessing ]) detectMax: [:version | version ]
]

{ #category : 'loading' }
MetacelloProject >> load: aVersionString [

	^(self version: aVersionString) load
]

{ #category : 'accessing' }
MetacelloProject >> loadType [
	"#atomic or #linear"

	^ loadType ifNil: [ #atomic ]
]

{ #category : 'accessing' }
MetacelloProject >> loadType: aSymbol [
	"#atomic or #linear"

	loadType := aSymbol
]

{ #category : 'private' }
MetacelloProject >> lookupVersion: aVersionString ifAbsent: aBlock [
    "please use version:...this is a private method"

    ^ self map
        at: aVersionString
        ifAbsent: [ 
            (MetacelloVersionDefinitionError project: self project versionString: aVersionString)
                exception: (self errorMap at: aVersionString ifAbsent: [ ^ aBlock value ]);
                signal ]
]

{ #category : 'accessing' }
MetacelloProject >> map [
	versionMap ifNil: [ ^ Dictionary new ].
	^ versionMap
]

{ #category : 'accessing' }
MetacelloProject >> map: aDictionary [

	versionMap := aDictionary
]

{ #category : 'spec classes' }
MetacelloProject >> packageSpec [

	^ self packageSpecClass for: self
]

{ #category : 'spec classes' }
MetacelloProject >> packageSpecClass [

	^MetacelloPackageSpec
]

{ #category : 'spec classes' }
MetacelloProject >> packagesSpec [

	^ MetacelloPackagesSpec for: self
]

{ #category : 'private' }
MetacelloProject >> platformAttributes [

	 ^self projectPlatformAttributes
]

{ #category : 'private' }
MetacelloProject >> pragmaKeywords [

	^#(version:attribute: blessing:attribute: description:attribute: required:attribute: groups:attribute: doits:attribute: projectPackage:attribute: packages:attribute: repositories:attribute: )
]

{ #category : 'printing' }
MetacelloProject >> printOn: aStream [

	| label |
	self configuration class printOn: aStream.
	aStream nextPut: $(.
	self versions do: [ :vrsn |
		aStream nextPutAll: vrsn versionString.
		vrsn spec ifNotNil: [ (label := vrsn spec projectLabel) isEmpty ifFalse: [ aStream nextPutAll: ' [' , label , ']' ] ].
		aStream
			nextPut: $,;
			space ].
	aStream nextPut: $)
]

{ #category : 'accessing' }
MetacelloProject >> project [

	^self
]

{ #category : 'private' }
MetacelloProject >> projectAttributes [

	^ projectAttributes ifNil: [ #(  ) ]
]

{ #category : 'private' }
MetacelloProject >> projectAttributes: aList [

	projectAttributes := aList
]

{ #category : 'development support' }
MetacelloProject >> projectPackage [

	self configuration class mcWorkingCopy ifNotNil: [ :workingCopy |
		| pkgSpec repo |
		pkgSpec := self packageSpec
			           name: workingCopy packageName;
			           yourself.
		repo := workingCopy repositoryGroup repositories
			        detect: [ :each | each ~~ MCCacheRepository default ]
			        ifNone: [
				        MetacelloNotification signal: 'Using cache repository for ' , self label , ' project package'.
				        MCCacheRepository default ].
		pkgSpec repository: (repo asRepositorySpecFor: self).
		^ pkgSpec ].
	^ nil
]

{ #category : 'private' }
MetacelloProject >> projectPlatformAttributes [

	| list aBlock |
	list := OrderedCollection new.
	(aBlock := self projectAttributes) ifNotNil: [ list addAll: aBlock value ].
	^ self defaultPlatformAttributes , list
]

{ #category : 'spec classes' }
MetacelloProject >> projectReferenceSpec [

	^ MetacelloProjectReferenceSpec for: self
]

{ #category : 'spec classes' }
MetacelloProject >> projectSpec [

	^ MetacelloMCNamelessProjectSpec for: self
]

{ #category : 'spec classes' }
MetacelloProject >> repositoriesSpec [

	^self repositoriesSpecClass for: self
]

{ #category : 'spec classes' }
MetacelloProject >> repositoriesSpecClass [

	^MetacelloRepositoriesSpec
]

{ #category : 'spec classes' }
MetacelloProject >> repositorySpec [

	^self repositorySpecClass for: self
]

{ #category : 'spec classes' }
MetacelloProject >> repositorySpecClass [

	^MetacelloRepositorySpec
]

{ #category : 'development support' }
MetacelloProject >> setBaselineRepositoryDescription: aListOrRepositoryDescriptions [
    "noop "
]

{ #category : 'private' }
MetacelloProject >> sortedAndFilteredVersions [

		^(self map values asArray sort: [:a :b | a >= b ]) select: [:vrsn | (#(structural broken baseline) includes: vrsn blessing) not ].
]

{ #category : 'versions' }
MetacelloProject >> stableVersion [

	^self version: #stable
]

{ #category : 'accessing' }
MetacelloProject >> symbolicVersionMap [

	^symbolicVersionMap
]

{ #category : 'accessing' }
MetacelloProject >> symbolicVersionMap: aDictionary [

	symbolicVersionMap := aDictionary
]

{ #category : 'versions' }
MetacelloProject >> symbolicVersionSymbols [

	^self symbolicVersionMap keys asArray sort: [:a :b | a <= b ]
]

{ #category : 'spec classes' }
MetacelloProject >> valueHolderSpec [

	^ MetacelloValueHolderSpec for: self
]

{ #category : 'versions' }
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

{ #category : 'versions' }
MetacelloProject >> version: aVersionString ifAbsent: aBlock [

	^[ self version: aVersionString ] on: MetacelloVersionDoesNotExistError do: [:ex | aBlock value ].
]

{ #category : 'versions' }
MetacelloProject >> versionNumberClass [

	^ versionNumberClass ifNil: [ versionNumberClass := MetacelloVersionNumber ]
]

{ #category : 'versions' }
MetacelloProject >> versionNumberClass: aClass [
    versionNumberClass := aClass
]

{ #category : 'spec classes' }
MetacelloProject >> versionSpec [

	^ MetacelloVersionSpec for: self
]

{ #category : 'versions' }
MetacelloProject >> versions [

	^self map values asArray sort: [:a :b | a <= b ]
]
