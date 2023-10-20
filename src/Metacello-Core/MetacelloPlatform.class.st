Class {
	#name : 'MetacelloPlatform',
	#superclass : 'Object',
	#instVars : [
		'bypassProgressBars'
	],
	#classVars : [
		'Current'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'accessing' }
MetacelloPlatform class >> current [
	Current
		ifNil: [Current := MetacelloPlatform new].
	^ Current
]

{ #category : 'class initialization' }
MetacelloPlatform class >> initialize [
  "noop ... use #select"
]

{ #category : 'initialize-release' }
MetacelloPlatform class >> select [
  Current := self new
]

{ #category : 'initialize-release' }
MetacelloPlatform class >> unselect [
  MetacelloPlatform current class = self
    ifTrue: [ Current := nil ]
]

{ #category : 'accessing' }
MetacelloPlatform >> bypassProgressBars [

	bypassProgressBars == nil ifTrue: [ bypassProgressBars := false ].
	^ bypassProgressBars
]

{ #category : 'accessing' }
MetacelloPlatform >> bypassProgressBars: anObject [
	bypassProgressBars := anObject
]

{ #category : 'caching' }
MetacelloPlatform >> clearCurrentVersionCache [
	MetacelloClearStackCacheNotification signal: #(#currentVersion #currentVersionAgainst: #currentVersionInfo)
]

{ #category : 'github/bitbucket support' }
MetacelloPlatform >> closeZipArchive: anArchive [
	"do nothing"
]

{ #category : 'notification' }
MetacelloPlatform >> collection: aCollection do: aBlock displaying: aString [

	aCollection do: aBlock
]

{ #category : 'user interaction' }
MetacelloPlatform >> confirm: aString [
	
	^(Smalltalk hasClassNamed: #UIManager)
		ifTrue: [ (Smalltalk classNamed: #UIManager) default perform: #confirm: with: aString ]
		ifFalse: [
			"throw warning and answer true, if no way to announce"
			Warning signal: aString.
			true ]
]

{ #category : 'reflection' }
MetacelloPlatform >> copyClass: oldClass as: newName inCategory: newCategoryName [

	self subclassResponsibility
]

{ #category : 'repository creation' }
MetacelloPlatform >> createRepository: aRepositorySpec [

	^ MCRepository newRepositoryFromSpec: aRepositorySpec
]

{ #category : 'package cache' }
MetacelloPlatform >> defaultPackageCache [
  "Apparently the MCCacheRepository api is changing in Pharo"

  ^ MCCacheRepository default
]

{ #category : 'attributes' }
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

{ #category : 'scripting' }
MetacelloPlatform >> defaultRepositoryDescription [
    ^ 'http://www.squeaksource.com/MetacelloRepository'
]

{ #category : 'file system' }
MetacelloPlatform >> directoryFromPath: adirectoryPath relativeTo: anotherDirectoryPath [
	"Get a handle on the following path: anotherDirectoryPath/adirectoryPath"
	<ignoreNotImplementedSelectors: #(directoryNamed:)>

	^ anotherDirectoryPath directoryNamed: adirectoryPath
]

{ #category : 'notification' }
MetacelloPlatform >> do: aBlock displaying: aString [

	aBlock value
]

{ #category : 'github/bitbucket support' }
MetacelloPlatform >> downloadJSON: url eTagsCache: eTagsCache eTagsKey: eTagsKey username: username pass: pass [
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  "To minimize GitHub rate limiting problems, we are caching the ETags for the tag request: https://developer.github.com/v3/#conditional-requests"

  ^ self downloadJSON: url username: username pass: pass
]

{ #category : 'github/bitbucket support' }
MetacelloPlatform >> downloadJSON: url username: username pass: pass [
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  self subclassResponsibility
]

{ #category : 'github/bitbucket support' }
MetacelloPlatform >> downloadZipArchive: url to: outputFileName [
  "download zip archive from <url> into <outputFileName>"

  self subclassResponsibility
]

{ #category : 'file system' }
MetacelloPlatform >> ensureDirectoryExists: aDirectoryHandle [
	"Ensure the directory exists."
	<ignoreNotImplementedSelectors: #(assureExistence)>

	^ aDirectoryHandle assureExistence ; yourself
]

{ #category : 'github/bitbucket support' }
MetacelloPlatform >> extractRepositoryFrom: zipFile to: directory [
    "unzip <zipFile> into <directory>"

    self subclassResponsibility
]

{ #category : 'repository creation' }
MetacelloPlatform >> extractTypeFromDescription: description [

	description ifNil: [ ^ nil ].
	((description beginsWith: '/') or: [ description second = $: ]) ifTrue: [ ^ 'directory' ].
	(description beginsWith: 'dictionary://') ifTrue: [ ^ 'dictionary' ].
	(description beginsWith: 'filetree://') ifTrue: [ ^ 'filetree' ].
	(description beginsWith: 'tonel://') ifTrue: [ ^ 'tonel' ].
	(description beginsWith: 'github://') ifTrue: [ ^ 'github' ].
	(description beginsWith: 'gitorious://') ifTrue: [ ^ 'gitorious' ].
	(description beginsWith: 'bitbucket://') ifTrue: [ ^ 'bitbucket' ].
	^ 'http'
]

{ #category : 'git support' }
MetacelloPlatform >> gitBranchNameFor: aDirectory [
  "return current branch for git repository located at <aDirectory>, if <aDirectory> is not a git repository, return nil"

  ^ nil
]

{ #category : 'git support' }
MetacelloPlatform >> gitCommitShaFor: aDirectory [
  "return SHA for git repository located at <aDirectory>, if <aDirectory> is not a git repository, return nil"

  ^ nil
]

{ #category : 'reflection' }
MetacelloPlatform >> globalNamed: globalName [

	^Smalltalk at: globalName
]

{ #category : 'reflection' }
MetacelloPlatform >> globalNamed: globalName ifAbsent: absentBlock [

	^Smalltalk at: globalName ifAbsent: absentBlock
]

{ #category : 'monticello' }
MetacelloPlatform >> newVersionForWorkingCopy: aWorkingCopy [
	<ignoreNotImplementedSelectors: #(newVersion)>
  ^ aWorkingCopy newVersion
]

{ #category : 'file system' }
MetacelloPlatform >> parentDirectoryOf: aFileHandler [
	"Get the parent directory of this file."
	<ignoreNotImplementedSelectors: #(containingDirectory)>

	^ aFileHandler containingDirectory
]

{ #category : 'caching' }
MetacelloPlatform >> primeStackCacheWith: aDictionary doing: noArgBlock [

	self 
		useStackCacheDuring: [:dict | ^noArgBlock value ] 
		defaultDictionary: aDictionary
]

{ #category : 'github/bitbucket support' }
MetacelloPlatform >> processPID [
  "return pid of process as string if available"

  ^ ''
]

{ #category : 'file system' }
MetacelloPlatform >> recursiveDelete: aDirectory [
	"delete this directory and all children of it"

	^  aDirectory recursiveDelete
]

{ #category : 'repository creation' }
MetacelloPlatform >> registerPackageNamed: packageName [
	<ignoreNotImplementedSelectors: #(registerPackageName:)>
  ^ (Smalltalk at: #'PackageInfo') registerPackageName: packageName
]

{ #category : 'system' }
MetacelloPlatform >> selectorForPragma: aPragma [
	^ aPragma selector
]

{ #category : 'caching' }
MetacelloPlatform >> stackCacheFor: cacheName at: key doing: aBlock [

	^self stackCacheFor: cacheName cacheClass: Dictionary at: key doing: aBlock
]

{ #category : 'caching' }
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

{ #category : 'utilities' }
MetacelloPlatform >> string: aString includesSubstring: aSubstring [
	<ignoreNotImplementedSelectors: #(includesSubString:)>

	"abstract String>>includesSubstring: to have the same message on all supported platforms."

	^aString includesSubString: aSubstring
]

{ #category : 'file system' }
MetacelloPlatform >> tempFileFor: aName suffix: aSuffixString [
	^ '/tmp/' , aName , aSuffixString
]

{ #category : 'utilities' }
MetacelloPlatform >> timestamp [

	^DateAndTime now printString
]

{ #category : 'caching' }
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
