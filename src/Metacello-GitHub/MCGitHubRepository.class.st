Class {
	#name : #MCGitHubRepository,
	#superclass : #MCGitBasedNetworkRepository,
	#classVars : [
		'CacheDirectory',
		'DownloadCache',
		'ETagsCache'
	],
	#category : #'Metacello-GitHub'
}

{ #category : #accessing }
MCGitHubRepository class >> basicDescription [
  ^ 'github'
]

{ #category : #'instance creation' }
MCGitHubRepository class >> basicFromUrl: aZnUrl [ 
	^ self location: aZnUrl asString
]

{ #category : #private }
MCGitHubRepository class >> cacheDirectory [
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  self resetCacheDirectoryIfInvalid.
  ^ CacheDirectory ifNil: [ self defaultCacheDirectory ].
]

{ #category : #private }
MCGitHubRepository class >> cacheDirectory: aDirectory [
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  CacheDirectory := aDirectory
]

{ #category : #'instance creation' }
MCGitHubRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createGithubRepository: aRepositorySpec
]

{ #category : #accessing }
MCGitHubRepository class >> downloadCache [
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  DownloadCache ifNil: [ DownloadCache := Dictionary new ].
  ^ DownloadCache
]

{ #category : #accessing }
MCGitHubRepository class >> eTagsCache [
  "To avoid GitHub rate limiting problems, we are caching the ETags for the tag request: https://developer.github.com/v3/#conditional-requests"

  ETagsCache ifNil: [ ETagsCache := Dictionary new ].
  ^ ETagsCache
]

{ #category : #initialization }
MCGitHubRepository class >> flushDownloadCache [
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  "self flushDownloadCache"

  DownloadCache := nil
]

{ #category : #initialization }
MCGitHubRepository class >> flushETagsCache [
  "mainly needed for tests .... but if you want to pare down object consumption..."

  "self flushETagsCache"

  ETagsCache := nil
]

{ #category : #testing }
MCGitHubRepository class >> isAvailableFor: type [
	^ type = 'github'
]

{ #category : #testing }
MCGitHubRepository class >> isEnabled [

	^true
]

{ #category : #private }
MCGitHubRepository class >> projectZipUrlFor: projectPath versionString: versionString [
  ^ 'https://github.com/' , projectPath , '/zipball/' , versionString
]

{ #category : #private }
MCGitHubRepository class >> resetCacheDirectoryIfInvalid [
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  "Reset if invalid"

  CacheDirectory notNil
    and: [ 
      (MCFileTreeFileUtils current directoryExists: CacheDirectory)
        ifFalse: [ CacheDirectory := nil ] ]
]

{ #category : #accessing }
MCGitHubRepository class >> urlSchemes [
	^ #(github)
]

{ #category : #private }
MCGitHubRepository >> downloadJSONTags [
  | tagsUrl jsonObject |
  tagsUrl := self projectTagsUrlFor: self projectPath.
  jsonObject := MetacelloPlatform current
    downloadJSON: tagsUrl
    eTagsCache: self class eTagsCache
    eTagsKey: self projectPath
    username: self class siteUsername
    pass: self class sitePassword.
  ^ self normalizeTagsData: jsonObject
]

{ #category : #private }
MCGitHubRepository >> normalizeTagsData: jsonObject [
  "return a dictionay mapping the tag name to the commit SHA"

  | tagDict |
  jsonObject isDictionary
    ifTrue: [ 
      jsonObject
        at: 'message'
        ifPresent: [ :errorString | 
          self
            error:
              'Error accessing tags for github project: ' , self projectPath printString
                , ' -> ' , errorString printString ] ].
  tagDict := Dictionary new.
  jsonObject
    do: [ :tagObject | 
      | tag commit |
      tag := tagObject at: 'name'.
      commit := tagObject at: 'commit'.
      tagDict at: tag put: (commit at: 'sha') ].
  ^ tagDict
]

{ #category : #private }
MCGitHubRepository >> projectTagsUrlFor: projectPath2 [
  ^ 'https://api.github.com/repos/' , projectPath2 , '/tags'
]
