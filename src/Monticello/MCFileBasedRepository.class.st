"
A file based metacello repository
"
Class {
	#name : #MCFileBasedRepository,
	#superclass : #MCRepository,
	#instVars : [
		'cache',
		'allFileNames',
		'cacheFileNames'
	],
	#category : #'Monticello-Repositories'
}

{ #category : #'instance creation' }
MCFileBasedRepository class >> basicFromUrl: aZnUrl [ 
	^ MCDirectoryRepository new directory: aZnUrl asFileReference
]

{ #category : #cleanup }
MCFileBasedRepository class >> cleanUp [
	"Flush caches"

	self flushAllCaches.
]

{ #category : #actions }
MCFileBasedRepository class >> flushAllCaches [
	self allSubInstancesDo: [:ea | ea flushCache]
]

{ #category : #settings }
MCFileBasedRepository class >> monticelloSettingsOn: aBuilder [  
	<systemsettings>  
	(aBuilder group: #monticello)
		label: 'Monticello';
		parent: #SCM;
		description: 'All Monticello settings';
		with: [ 
			(aBuilder setting: #defaultDirectoryName)
				type: #Directory;
				target: MCDirectoryRepository;
				description: 'The path of a directory where you want to start out when choosing local repository locations' ;
				default: FileSystem workingDirectory fullName;
				label: 'Default local repository directory'.
			(aBuilder setting: #cacheDirectory)
				type: #Directory;
				target: MCCacheRepository;
				default: (FileLocator localDirectory / 'package-cache') asFileReference;
				description: 'The path of the local repository cache';
				label: 'Local cache directory'.	
				
				].
]

{ #category : #accessing }
MCFileBasedRepository class >> urlSchemes [
	^ #(file)
]

{ #category : #adding }
MCFileBasedRepository >> addVersionInformationExtractedFrom: readableFileName to: versions [
	| name |
	name := (readableFileName copyUpToLast: $.) copyUpTo: $(.
	name last isDigit
		ifTrue: [ 
			versions
				add:
					{(name copyUpToLast: $-).	"pkg name"
					((name copyAfterLast: $-) copyUpTo: $.).	"user"
					(((name copyAfterLast: $-) copyAfter: $.) asInteger ifNil: [ 0 ]).	"version"
					readableFileName }]
]

{ #category : #accessing }
MCFileBasedRepository >> allFileNames [
	cacheFileNames == true ifFalse: [ ^ self loadAllFileNames ].
	^ allFileNames ifNil: [
			allFileNames := self loadAllFileNames]
]

{ #category : #accessing }
MCFileBasedRepository >> allFileNamesForVersionNamed: aString [
	^ self filterFileNames: self readableFileNames forVersionNamed: aString
]

{ #category : #accessing }
MCFileBasedRepository >> allFileNamesOrCache [
	cacheFileNames == true ifFalse: [ ^ self allFileNames ].
	^ allFileNames ifNil: [
			allFileNames := self allFileNames]
]

{ #category : #accessing }
MCFileBasedRepository >> allVersionNames [
	^ self readableFileNames collect: [:ea | self versionNameFromFileName: ea]
]

{ #category : #storing }
MCFileBasedRepository >> basicStoreVersion: aVersion [
	self
		writeStreamForFileNamed: aVersion fileName
		do: [:s | aVersion fileOutOn: s].
	aVersion isCacheable ifTrue: [
		cache ifNil: [cache := Dictionary new].
		cache at: aVersion fileName put: aVersion].

]

{ #category : #accessing }
MCFileBasedRepository >> cache [
	^ cache ifNil: [cache := Dictionary new]
]

{ #category : #caching }
MCFileBasedRepository >> cacheAllFileNamesDuring: aBlock [
	cacheFileNames == true ifTrue: [ ^ aBlock value ].
	allFileNames := nil.
	cacheFileNames := true.	
	^ aBlock ensure: [
		allFileNames := nil.
		cacheFileNames := false]
]

{ #category : #caching }
MCFileBasedRepository >> cachedFileNames [
	^cache == nil
		ifTrue: [#()]
		ifFalse: [cache keys]
]

{ #category : #testing }
MCFileBasedRepository >> canReadFileNamed: aString [
	| reader |
	reader := MCVersionReader readerClassForFileNamed: aString.
	^ reader notNil
]

{ #category : #accessing }
MCFileBasedRepository >> closestAncestorVersionFor: anAncestry ifNone: errorBlock [
	^ self
		cacheAllFileNamesDuring: [ 
			anAncestry breadthFirstAncestorsDo: [ :ancestorInfo | (self notCachedVersionWithInfo: ancestorInfo) ifNotNil: [ :v | ^ v ] ].
			^ errorBlock value ]
]

{ #category : #accessing }
MCFileBasedRepository >> filterFileNames: aCollection forVersionNamed: aString [
	^ aCollection select: [:ea | (self versionNameFromFileName: ea) = aString] 
]

{ #category : #private }
MCFileBasedRepository >> flushCache [
	cache := nil
]

{ #category : #testing }
MCFileBasedRepository >> includesFileNamed: aString [
	"slow default implementation"
	^ self allFileNames includes: aString
]

{ #category : #testing }
MCFileBasedRepository >> includesVersionNamed: aString [
	^ self allVersionNames includes: aString
]

{ #category : #interface }
MCFileBasedRepository >> loadAllFileNames [
	self subclassResponsibility
]

{ #category : #private }
MCFileBasedRepository >> loadNotCachedVersionFromFileNamed: aString [
	
	^ self versionReaderForFileNamed: aString do: [:r | r version]
]

{ #category : #private }
MCFileBasedRepository >> loadVersionFromFileNamed: aString [

	(MCCacheRepository uniqueInstance includesFileNamed: aString)
		ifTrue: [ ^ MCCacheRepository uniqueInstance loadVersionFromFileNamed: aString].
	
	^ self versionReaderForFileNamed: aString do: [:r | r version]
]

{ #category : #loading }
MCFileBasedRepository >> loadVersionInfoFromFileNamed: aString [
	^ self versionReaderForFileNamed: aString do: [:r | r info]
	
]

{ #category : #caching }
MCFileBasedRepository >> maxCacheSize [
	^ 512
]

{ #category : #loading }
MCFileBasedRepository >> notCachedVersionWithInfo: aVersionInfo [ 
	(self allFileNamesForVersionNamed: aVersionInfo name)
		do: [ :fileName | 
			| version |
			version := self versionFromRepositoryFromFileNamed: fileName.
			version info = aVersionInfo
				ifTrue: [ ^ version ] ].
	^ nil
]

{ #category : #loading }
MCFileBasedRepository >> pathForFileNamed: aFileName [
  ^ aFileName
]

{ #category : #accessing }
MCFileBasedRepository >> possiblyNewerVersionsOfAnyOf: someVersions [
	| pkgs |
	pkgs := Dictionary new.

	someVersions do: [:aVersionInfo |
		pkgs at: (aVersionInfo name copyUpToLast: $-)
			put: (aVersionInfo name copyAfterLast: $.) asNumber].

	^[self allVersionNames select: [:each |
		(pkgs at: (each copyUpToLast: $-) ifPresent: [:verNumber |
			verNumber < (each copyAfterLast: $.) asNumber
				or: [verNumber = (each copyAfterLast: $.) asNumber
					and: [someVersions noneSatisfy: [:v | v name = each]]]]) == true]
	] on: Error do: [:ex | ex return: #()]
]

{ #category : #accessing }
MCFileBasedRepository >> readableFileNames [
	| all cached new emptyFilenamelength |
	"<hyphenated-package-name>.<dotted.branch.tag>-<initials>.<count>.mcz"
	emptyFilenamelength := 'P-i.c.mcz' size.
	all := self allFileNames.	"from repository"
		
	all := all reject: [ :each |  each size < emptyFilenamelength].
		"first stupid way to filter first level broken files. Ideally we should 
		remove any files not following the naming pattern: PackageName-author.number[(branch)].mcz"
	
	"Another stupid way to remove filename when using filetree repositories."
	all := all reject: [ :each | each beginsWith: '.filetree' ].
	
	cached := self cachedFileNames.	"in memory"
	new := all difference: cached.
	^ (cached asArray, new)
		select: [:ea | self canReadFileNamed: ea]
]

{ #category : #caching }
MCFileBasedRepository >> resizeCache: aDictionary [
	[aDictionary size <= self maxCacheSize] whileFalse:
		[aDictionary removeKey: aDictionary keys atRandom]
]

{ #category : #storing }
MCFileBasedRepository >> storeVersion: aVersion [

	(self includesFileNamed: aVersion fileName) ifTrue: [ ^ self ].
	^ super storeVersion: aVersion.
]

{ #category : #private }
MCFileBasedRepository >> updateCachedVersionFromFileName: aString with: aVersion [
	self resizeCache: cache.
	(aVersion notNil and: [aVersion isCacheable]) ifTrue: [cache at: aString put: aVersion].

]

{ #category : #accessing }
MCFileBasedRepository >> versionFrom: aVersionReferenceString [
	^ self loadVersionFromFileNamed: aVersionReferenceString , '.mcz'
]

{ #category : #accessing }
MCFileBasedRepository >> versionFromFileNamed: aFileName [
	| version |
	version := self cache
			 at: aFileName 
			 ifAbsent: [ self loadVersionFromFileNamed: aFileName ].
	self updateCachedVersionFromFileName: aFileName with: version.
	^ version
]

{ #category : #accessing }
MCFileBasedRepository >> versionFromRepositoryFromFileNamed: aFileName [
	| version |
	version := self cache
			 at: aFileName 
			 ifAbsent: [ self loadNotCachedVersionFromFileNamed: aFileName ].
	self updateCachedVersionFromFileName: aFileName with: version.
	^ version
]

{ #category : #accessing }
MCFileBasedRepository >> versionInfoFromFileNamed: aFileName [
	self cache 
		at: aFileName
		ifPresent: [:version | ^ version info].
	^ self loadVersionInfoFromFileNamed: aFileName
]

{ #category : #accessing }
MCFileBasedRepository >> versionNameFromFileName: aFileName [
	^ (aFileName copyUpToLast: $.) copyUpTo: $(
]

{ #category : #actions }
MCFileBasedRepository >> versionReaderForFileNamed: aString do: aBlock [
^ self
		readStreamForFileNamed: aString
		do: [:s |
			(MCVersionReader readerClassForFileNamed: aString) ifNotNil:
				[:class | aBlock value: (class on: s fileName: (self pathForFileNamed: aString))]]
]

{ #category : #interface }
MCFileBasedRepository >> versionWithInfo: aVersionInfo ifAbsent: errorBlock [
	"get a version for the given versionInfo. always query first the packageCache and only then try to load the version from the remote location"
	^ MCCacheRepository uniqueInstance
		versionWithInfo: aVersionInfo 
		ifAbsent: [
			(self allFileNamesForVersionNamed: aVersionInfo name) do:
				[:fileName | | version |
				version := self versionFromRepositoryFromFileNamed: fileName.
				version info = aVersionInfo ifTrue: [^ version]].
			^ errorBlock value].
]

{ #category : #accessing }
MCFileBasedRepository >> versionsWithPackageNames [
	| versions |
	versions := OrderedCollection new.
	self readableFileNames
		do: [ :each | 
		      self addVersionInformationExtractedFrom: each to: versions ].
	^ versions
]

{ #category : #storing }
MCFileBasedRepository >> writeStreamForFileNamed: aString do: aBlock [
	^ self writeStreamForFileNamed: aString replace: false do: aBlock
]
