"
A file based metacello repository
"
Class {
	#name : 'MCFileBasedRepository',
	#superclass : 'MCRepository',
	#instVars : [
		'cache'
	],
	#category : 'Monticello-Repositories',
	#package : 'Monticello',
	#tag : 'Repositories'
}

{ #category : 'instance creation' }
MCFileBasedRepository class >> basicFromUrl: aZnUrl [ 
	^ MCDirectoryRepository new directory: aZnUrl asFileReference
]

{ #category : 'cleanup' }
MCFileBasedRepository class >> cleanUp [
	"Flush caches"

	self flushAllCaches.
]

{ #category : 'actions' }
MCFileBasedRepository class >> flushAllCaches [
	self allSubInstancesDo: [:ea | ea flushCache]
]

{ #category : 'settings' }
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
				description: 'The path of a directory where you want to start out when choosing local repository locations';
				default: FileSystem workingDirectory fullName;
				label: 'Default local repository directory' ]
]

{ #category : 'accessing' }
MCFileBasedRepository class >> urlSchemes [
	^ #(file)
]

{ #category : 'accessing' }
MCFileBasedRepository >> allFileNames [

	^ self loadAllFileNames
]

{ #category : 'accessing' }
MCFileBasedRepository >> allFileNamesForVersionNamed: aString [
	^ self filterFileNames: self readableFileNames forVersionNamed: aString
]

{ #category : 'accessing' }
MCFileBasedRepository >> allVersionNames [
	^ self readableFileNames collect: [:ea | self versionNameFromFileName: ea]
]

{ #category : 'storing' }
MCFileBasedRepository >> basicStoreVersion: aVersion [

	self writeStreamForFileNamed: aVersion fileName do: [ :s | aVersion fileOutOn: s ].

	cache ifNil: [ cache := Dictionary new ].
	cache at: aVersion fileName put: aVersion
]

{ #category : 'accessing' }
MCFileBasedRepository >> cache [
	^ cache ifNil: [cache := Dictionary new]
]

{ #category : 'caching' }
MCFileBasedRepository >> cachedFileNames [

	^ cache
		  ifNil: [ #(  ) ]
		  ifNotNil: [ cache keys ]
]

{ #category : 'testing' }
MCFileBasedRepository >> canReadFileNamed: aString [
	| reader |
	reader := MCVersionReader readerClassForFileNamed: aString.
	^ reader isNotNil
]

{ #category : 'fetching' }
MCFileBasedRepository >> fetchPackageNamed: aName [
	"Do nothing when we are not remote."

	
]

{ #category : 'accessing' }
MCFileBasedRepository >> filterFileNames: aCollection forVersionNamed: aString [
	^ aCollection select: [:ea | (self versionNameFromFileName: ea) = aString] 
]

{ #category : 'private' }
MCFileBasedRepository >> flushCache [
	cache := nil
]

{ #category : 'testing' }
MCFileBasedRepository >> includesFileNamed: aString [
	"slow default implementation"
	^ self allFileNames includes: aString
]

{ #category : 'testing' }
MCFileBasedRepository >> includesVersionNamed: aString [
	^ self allVersionNames includes: aString
]

{ #category : 'interface' }
MCFileBasedRepository >> loadAllFileNames [
	self subclassResponsibility
]

{ #category : 'private' }
MCFileBasedRepository >> loadNotCachedVersionFromFileNamed: aString [
	
	^ self versionReaderForFileNamed: aString do: [:r | r version]
]

{ #category : 'accessing' }
MCFileBasedRepository >> loadPackageNamed: aString intoLoader: aMCVersionLoader [ 

	| found |
	found := self packageNamed: aString.
	aMCVersionLoader addVersion: found.
	^ { found . self }
]

{ #category : 'private' }
MCFileBasedRepository >> loadVersionFromFileNamed: aString [

	^ self versionReaderForFileNamed: aString do: [ :r | r version ]
]

{ #category : 'loading' }
MCFileBasedRepository >> loadVersionInfoFromFileNamed: aString [
	^ self versionReaderForFileNamed: aString do: [:r | r info]
	
]

{ #category : 'caching' }
MCFileBasedRepository >> maxCacheSize [
	^ 512
]

{ #category : 'accessing' }
MCFileBasedRepository >> packageDescriptionsFromReadableFileNames [
	^ self readableFileNames
		collect: [ :each | 
			| name |
			name := (each copyUpToLast: $.) copyUpTo: $(.
			name last isDigit
				ifFalse: [ 
					{name.
					''.
					''.
					each} ]
				ifTrue: [ 
					| packageName author versionNumber |
					packageName := name copyUpToLast: $-.
					author := (name copyAfterLast: $-) copyUpTo: $..
					versionNumber := ((name copyAfterLast: $-) copyAfter: $.) asInteger ifNil: [ 0 ].
					{packageName.
					author.
					versionNumber.
					each} ] ]
]

{ #category : 'accessing' }
MCFileBasedRepository >> packageNamed: aName ifPresent: presentBlock ifAbsent: absentBlock [

	| versionName |
	versionName := self packageDescriptionsFromReadableFileNames
		               detect: [ :e | e first = aName ]
		               ifNone: [ ^ absentBlock value ].
	^ presentBlock value: (self versionFromFileNamed: versionName last)
]

{ #category : 'loading' }
MCFileBasedRepository >> pathForFileNamed: aFileName [
  ^ aFileName
]

{ #category : 'actions' }
MCFileBasedRepository >> readStreamForFileNamed: aString do: aBlock [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
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

{ #category : 'caching' }
MCFileBasedRepository >> resizeCache: aDictionary [
	[aDictionary size <= self maxCacheSize] whileFalse:
		[aDictionary removeKey: aDictionary keys atRandom]
]

{ #category : 'storing' }
MCFileBasedRepository >> storeVersion: aVersion [

	(self includesFileNamed: aVersion fileName) ifTrue: [ ^ self ].
	^ super storeVersion: aVersion.
]

{ #category : 'private' }
MCFileBasedRepository >> updateCachedVersionFromFileName: aString with: aVersion [

	self resizeCache: cache.
	aVersion ifNotNil: [ cache at: aString put: aVersion ]
]

{ #category : 'accessing' }
MCFileBasedRepository >> versionFromFileNamed: aFileName [
	| version |
	version := self cache
			 at: aFileName 
			 ifAbsent: [ self loadVersionFromFileNamed: aFileName ].
	self updateCachedVersionFromFileName: aFileName with: version.
	^ version
]

{ #category : 'accessing' }
MCFileBasedRepository >> versionFromRepositoryFromFileNamed: aFileName [
	| version |
	version := self cache
			 at: aFileName 
			 ifAbsent: [ self loadNotCachedVersionFromFileNamed: aFileName ].
	self updateCachedVersionFromFileName: aFileName with: version.
	^ version
]

{ #category : 'accessing' }
MCFileBasedRepository >> versionInfoFromFileNamed: aFileName [
	self cache 
		at: aFileName
		ifPresent: [:version | ^ version info].
	^ self loadVersionInfoFromFileNamed: aFileName
]

{ #category : 'accessing' }
MCFileBasedRepository >> versionNameFromFileName: aFileName [
	^ (aFileName copyUpToLast: $.) copyUpTo: $(
]

{ #category : 'actions' }
MCFileBasedRepository >> versionReaderForFileNamed: aString do: aBlock [
^ self
		readStreamForFileNamed: aString
		do: [:s |
			(MCVersionReader readerClassForFileNamed: aString) ifNotNil:
				[:class | aBlock value: (class on: s fileName: (self pathForFileNamed: aString))]]
]

{ #category : 'interface' }
MCFileBasedRepository >> versionWithInfo: aVersionInfo ifAbsent: errorBlock [
	"get a version for the given versionInfo"

	(self allFileNamesForVersionNamed: aVersionInfo name) do: [ :fileName |
		| version |
		version := self versionFromRepositoryFromFileNamed: fileName.
		version info = aVersionInfo ifTrue: [ ^ version ] ].
	^ errorBlock value
]

{ #category : 'storing' }
MCFileBasedRepository >> writeStreamForFileNamed: aString do: aBlock [

	^ self subclassResponsibility
]
