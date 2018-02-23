Class {
	#name : #MetacelloPlatform,
	#superclass : #Object,
	#instVars : [
		'bypassProgressBars',
		'bypassGoferLoadUpdateCategories'
	],
	#classVars : [
		'Current'
	],
	#category : #'Metacello-Core-Model'
}

{ #category : #accessing }
MetacelloPlatform class >> current [
	Current
		ifNil: [Current := MetacelloPlatform new].
	^ Current
]

{ #category : #'initialize-release' }
MetacelloPlatform class >> initialize [
  "noop ... use #select"
]

{ #category : #'initialize-release' }
MetacelloPlatform class >> select [
  Current := self new
]

{ #category : #'initialize-release' }
MetacelloPlatform class >> unselect [
  MetacelloPlatform current class = self
    ifTrue: [ Current := nil ]
]

{ #category : #utilities }
MetacelloPlatform >> authorName [

	Smalltalk at: #Author ifPresent: [:cl | ^cl perform: #initials ].
	^'no developer initials'
]

{ #category : #utilities }
MetacelloPlatform >> authorName: aString [
	"Primarily used for testing"

	self subclassResponsibility
]

{ #category : #accessing }
MetacelloPlatform >> bypassGoferLoadUpdateCategories [

	bypassGoferLoadUpdateCategories == nil ifTrue: [ bypassGoferLoadUpdateCategories := false ].
	^ bypassGoferLoadUpdateCategories
]

{ #category : #accessing }
MetacelloPlatform >> bypassGoferLoadUpdateCategories: anObject [
	bypassGoferLoadUpdateCategories := anObject
]

{ #category : #accessing }
MetacelloPlatform >> bypassProgressBars [

	bypassProgressBars == nil ifTrue: [ bypassProgressBars := false ].
	^ bypassProgressBars
]

{ #category : #accessing }
MetacelloPlatform >> bypassProgressBars: anObject [
	bypassProgressBars := anObject
]

{ #category : #caching }
MetacelloPlatform >> clearCurrentVersionCache [
	MetacelloClearStackCacheNotification signal: #(#currentVersion #currentVersionAgainst: #currentVersionInfo)
]

{ #category : #'package cache' }
MetacelloPlatform >> clearPackageCache [
  "make sure that the package cache is empty, by hook or by crook"

  | cache dir |
  cache := self defaultPackageCache.
  cache flushCache.	"GemStone and Pharo1.4"
  dir := cache directory.
  dir ~~ nil
    ifTrue: [ 
      "delete directory - Squeak and Pharo"
      [ self recursiveDelete: dir ]
        on: Error
        do: [ :ignore |  ] ]
]

{ #category : #notification }
MetacelloPlatform >> collection: aCollection do: aBlock displaying: aString [

	aCollection do: aBlock
]

{ #category : #'user interaction' }
MetacelloPlatform >> confirm: aString [
	
	^(Smalltalk hasClassNamed: #UIManager)
		ifTrue: [ (Smalltalk classNamed: #UIManager) default perform: #confirm: with: aString ]
		ifFalse: [
			"throw warning and answer true, if no way to announce"
			Warning signal: aString.
			true ]
]

{ #category : #reflection }
MetacelloPlatform >> copyClass: oldClass as: newName inCategory: newCategoryName [

	self subclassResponsibility
]

{ #category : #'repository creation' }
MetacelloPlatform >> createRepository: aRepositorySpec [
  | type |
  type := aRepositorySpec type.
  type = 'http'
    ifTrue: [ 
      ^ MCHttpRepository
        location: aRepositorySpec description
        user: aRepositorySpec username
        password: aRepositorySpec password ].
  type = 'directory'
    ifTrue: [ 
      ^ MCDirectoryRepository new
        directory: (self fileHandleOn: aRepositorySpec description) ].
  Smalltalk
    at: #'MCFileTreeRepository'
    ifPresent: [ :cl | 
      type = 'filetree'
        ifTrue: [ 
          | description headerSize |
          description := aRepositorySpec description.
          headerSize := 'filetree://' size.
          ^ cl new
            directory:
              (self
                fileHandleOn:
                  (aRepositorySpec description copyFrom: headerSize + 1 to: description size)) ] ].
  Smalltalk
    at: #'TonelRepository'
    ifPresent: [ :cl | 
      type = 'tonel'
        ifTrue: [ 
          | description headerSize |
          description := aRepositorySpec description.
          headerSize := 'tonel://' size.
          ^ cl new
            directory:
              (self
                fileHandleOn:
                  (aRepositorySpec description copyFrom: headerSize + 1 to: description size)) ] ].
  Smalltalk
    at: #'MCGitHubRepository'
    ifPresent: [ :cl | 
      type = 'github'
        ifTrue: [ ^ cl location: aRepositorySpec description ] ].
  Smalltalk
    at: #'MCBitbucketRepository'
    ifPresent: [ :cl | 
      type = 'bitbucket'
        ifTrue: [ ^ cl location: aRepositorySpec description ] ].
  Smalltalk
    at: #'MCGitoriousRepository'
    ifPresent: [ :cl | 
      type = 'gitorious'
        ifTrue: [ ^ cl location: aRepositorySpec description ] ].
  type = 'dictionary'
    ifTrue: [ 
      | description headerSize globalName |
      description := aRepositorySpec description.
      headerSize := 'dictionary://' size.
      globalName := (description copyFrom: headerSize + 1 to: description size)
        asSymbol.
      ^ Smalltalk
        at: globalName
        ifAbsent: [ 
          Smalltalk
            at: globalName
            put:
              (MCDictionaryRepository new
                description: description;
                yourself) ] ].
  ^ nil
]

{ #category : #'file system' }
MetacelloPlatform >> defaultDirectory [
	"Get the image default directory"
	
	^self fileDirectoryClass default
]

{ #category : #'package cache' }
MetacelloPlatform >> defaultPackageCache [
  "Apparently the MCCacheRepository api is changing in Pharo"

  ^ MCCacheRepository uniqueInstance
]

{ #category : #attributes }
MetacelloPlatform >> defaultPlatformAttributes [
  | versionString |
  Smalltalk
    at: #'SystemVersion'
    ifPresent: [ :cl | 
      versionString := cl current version.
      (versionString beginsWith: 'Pharo')
        ifTrue: [ ^ #(#'squeakCommon' #'pharo') ].
      (versionString beginsWith: 'Squeak')
        ifTrue: [ ^ #(#'squeakCommon' #'squeak') ].	"see http://code.google.com/p/metacello/issues/detail?id=146"
      (self string: versionString includesSubstring: 'Pharo')
        ifTrue: [ ^ #(#'squeakCommon' #'pharo') ].
      (self string: versionString includesSubstring: 'Squeak')
        ifTrue: [ ^ #(#'squeakCommon' #'squeak') ].
      self error: 'Unrecognized version of Squeak/Pharo: ' , versionString ].
  ^ #(#'gemstone')
]

{ #category : #scripting }
MetacelloPlatform >> defaultRepositoryDescription [
    ^ 'http://www.squeaksource.com/MetacelloRepository'
]

{ #category : #tests }
MetacelloPlatform >> defaultTimeout [
	"squeak compatability"
	^60
]

{ #category : #'file system' }
MetacelloPlatform >> deleteFileNamed: filePath [
	(self fileDirectoryClass on: filePath) containingDirectory deleteFileNamed: (self fileDirectoryClass localNameFor: filePath)
]

{ #category : #'file system' }
MetacelloPlatform >> directoryFromPath: adirectoryPath relativeTo: anotherDirectoryPath [
	"Get a handle on the following path: anotherDirectoryPath/adirectoryPath"
	^ anotherDirectoryPath directoryNamed: adirectoryPath
]

{ #category : #tests }
MetacelloPlatform >> disableUndefinedSybolUpdates [
  "noop for most platforms - needed for GemStone"

  ^ nil
]

{ #category : #notification }
MetacelloPlatform >> do: aBlock displaying: aString [

	aBlock value
]

{ #category : #'github/bitbucket support' }
MetacelloPlatform >> downloadErrorFileNameFor: zipFileName [
  | pid |
  pid := self processPID.
  ^ '/tmp/curl-' , pid , '-' , (zipFileName select: [ :c | c isAlphaNumeric ])
    , '.err'
]

{ #category : #'github/bitbucket support' }
MetacelloPlatform >> downloadJSON: url eTagsCache: eTagsCache eTagsKey: eTagsKey username: username pass: pass [
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  "To minimize GitHub rate limiting problems, we are caching the ETags for the tag request: https://developer.github.com/v3/#conditional-requests"

  ^ self downloadJSON: url username: username pass: pass
]

{ #category : #'github/bitbucket support' }
MetacelloPlatform >> downloadJSON: url username: username pass: pass [
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  self subclassResponsibility
]

{ #category : #'github/bitbucket support' }
MetacelloPlatform >> downloadZipArchive: url to: outputFileName [
  "download zip archive from <url> into <outputFileName>"

  self subclassResponsibility
]

{ #category : #'file system' }
MetacelloPlatform >> ensureDirectoryExists: aDirectoryHandle [
	"Ensure the directory exists."

	^ aDirectoryHandle assureExistence ; yourself
]

{ #category : #'github/bitbucket support' }
MetacelloPlatform >> extractRepositoryFrom: zipFile to: directory [
    "unzip <zipFile> into <directory>"

    self subclassResponsibility
]

{ #category : #'repository creation' }
MetacelloPlatform >> extractTypeFromDescription: description [
  description == nil
    ifTrue: [ ^ nil ].
  ((description beginsWith: '/') or: [ description second = $: ])
    ifTrue: [ ^ 'directory' ].
  (description beginsWith: 'dictionary://')
    ifTrue: [ ^ 'dictionary' ].
  (description beginsWith: 'filetree://')
    ifTrue: [ ^ 'filetree' ].
  (description beginsWith: 'tonel://')
    ifTrue: [ ^ 'tonel' ].
  (description beginsWith: 'github://')
    ifTrue: [ ^ 'github' ].
  (description beginsWith: 'gitorious://')
    ifTrue: [ ^ 'gitorious' ].
  (description beginsWith: 'bitbucket://')
    ifTrue: [ ^ 'bitbucket' ].
  ^ 'http'
]

{ #category : #'github/bitbucket support' }
MetacelloPlatform >> fileDirectoryClass [
	^ Smalltalk at: #FileDirectory
]

{ #category : #'file system' }
MetacelloPlatform >> fileHandleOn: aPath [
	"Get an handle on a file."

	^ self fileDirectoryClass on: aPath
]

{ #category : #'git support' }
MetacelloPlatform >> gitBranchNameFor: aDirectory [
  "return current branch for git repository located at <aDirectory>, if <aDirectory> is not a git repository, return nil"

  ^ nil
]

{ #category : #'git support' }
MetacelloPlatform >> gitCommitShaFor: aDirectory [
  "return SHA for git repository located at <aDirectory>, if <aDirectory> is not a git repository, return nil"

  ^ nil
]

{ #category : #reflection }
MetacelloPlatform >> globalNamed: globalName [

	^Smalltalk at: globalName
]

{ #category : #reflection }
MetacelloPlatform >> globalNamed: globalName ifAbsent: absentBlock [

	^Smalltalk at: globalName ifAbsent: absentBlock
]

{ #category : #monticello }
MetacelloPlatform >> newVersionForWorkingCopy: aWorkingCopy [
  ^ aWorkingCopy newVersion
]

{ #category : #'repository creation' }
MetacelloPlatform >> packageInfoFor: aMCWorkingCopy [
  ^ aMCWorkingCopy packageInfo
]

{ #category : #'file system' }
MetacelloPlatform >> parentDirectoryOf: aFileHandler [
	"Get the parent directory of this file."
	
	^ aFileHandler containingDirectory
]

{ #category : #caching }
MetacelloPlatform >> primeStackCacheFor: cacheName doing: noArgBlock defaultDictionary: aDictionary [

	self deprecated: 'use #primeStackCacheWith:doing:'.
	self 
		useStackCacheDuring: [:dict | | cache |
			cache := dict at: cacheName ifAbsent: [].
			cache == nil
				ifTrue: [ 
					cache := Dictionary new.
					dict at: cacheName put: cache ].
			^noArgBlock value ] 
		defaultDictionary: aDictionary
]

{ #category : #caching }
MetacelloPlatform >> primeStackCacheWith: aDictionary doing: noArgBlock [

	self 
		useStackCacheDuring: [:dict | ^noArgBlock value ] 
		defaultDictionary: aDictionary
]

{ #category : #'github/bitbucket support' }
MetacelloPlatform >> processPID [
  "return pid of process as string if available"

  ^ ''
]

{ #category : #'file system' }
MetacelloPlatform >> readStreamOn: aFileHandle do: aBlock [
	"Get a read stream on the file handle and execute some actions on it."
	| stream |
	stream := aFileHandle readStream.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
]

{ #category : #'file system' }
MetacelloPlatform >> recursiveDelete: aDirectory [
	"delete this directory and all children of it"

	^  aDirectory recursiveDelete
]

{ #category : #tests }
MetacelloPlatform >> reenableUndefinedSybolUpdates: undefinedSymbols [
  "noop for most platforms - needed for GemStone"


]

{ #category : #'repository creation' }
MetacelloPlatform >> registerPackageNamed: packageName [
  ^ (Smalltalk at: #'PackageInfo') registerPackageName: packageName
]

{ #category : #system }
MetacelloPlatform >> selectorForPragma: aPragma [
	^ aPragma selector
]

{ #category : #caching }
MetacelloPlatform >> stackCacheFor: cacheName at: key doing: aBlock [

	^self stackCacheFor: cacheName cacheClass: Dictionary at: key doing: aBlock
]

{ #category : #caching }
MetacelloPlatform >> stackCacheFor: cacheName cacheClass: cacheClass at: key doing: aBlock [

	self 
		useStackCacheDuring: [:dict | | cache |
			cache := dict at: cacheName ifAbsent: [].
			cache ~~ nil
				ifTrue: [ | value hasEntry |
					hasEntry := true.
					value := cache at: key ifAbsent: [ hasEntry := false ].
					hasEntry ifTrue: [ ^value ]]
				ifFalse: [ 
					cache := cacheClass new.
					dict at: cacheName put: cache ].
			^aBlock value: cache ] 
		defaultDictionary: nil
]

{ #category : #utilities }
MetacelloPlatform >> string: aString includesSubstring: aSubstring [
	"abstract String>>includesSubstring: to have the same message on all supported platforms."

	^aString includesSubString: aSubstring
]

{ #category : #system }
MetacelloPlatform >> suspendSystemUpdateEventsDuring: aBlock [
	"Wraps call to the system evetn manager to tell it that we want to suspend events during execution of aBlock"
	(Smalltalk at: #SystemChangeNotifier) uniqueInstance doSilently: aBlock
]

{ #category : #'file system' }
MetacelloPlatform >> tempFileFor: aName suffix: aSuffixString [
	^ '/tmp/' , aName , aSuffixString
]

{ #category : #utilities }
MetacelloPlatform >> timestamp [

	^DateAndTime now printString
]

{ #category : #transactions }
MetacelloPlatform >> transact: aBlock [
	"On GemStone, we want to optionally abort before command execution and commit after 
	 common execution. Other plaforms don't need to do anything special.
	 Returning out of block, skips commit."
	
	aBlock value
]

{ #category : #caching }
MetacelloPlatform >> useStackCacheDuring: aBlock defaultDictionary: defaultDictionary [
	| dict |
	dict := MetacelloStackCacheNotification signal.
	dict == nil
		ifTrue: [ 
			dict := defaultDictionary == nil
				ifTrue: [ Dictionary new ]
				ifFalse: [ defaultDictionary ] ].
	[ ^ aBlock value: dict ]
		on: MetacelloStackCacheNotification , MetacelloClearStackCacheNotification
		do: [ :ex | 
			(ex isKindOf: MetacelloStackCacheNotification)
				ifTrue: [ ex resume: dict ].
			(ex isKindOf: MetacelloClearStackCacheNotification)
				ifTrue: [ 
					| keys |
					keys := ex cacheNames.
					keys ifNil: [ keys := dict keys ].
					keys
						do: [ :k | 
							(dict includesKey: k)
								ifTrue: [ 
									| c |
									c := dict at: k.
									c keys do: [ :ck | c removeKey: ck ].
									dict removeKey: k ] ].
					ex resume ] ]
]
