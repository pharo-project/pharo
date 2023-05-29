Class {
	#name : #MCBitbucketRepository,
	#superclass : #MCPrivateHostableRepository,
	#category : #'Metacello-Bitbucket'
}

{ #category : #accessing }
MCBitbucketRepository class >> basicDescription [
  ^ 'bitbucket'
]

{ #category : #'instance creation' }
MCBitbucketRepository class >> basicFromUrl: aZnUrl [ 
	^ self location: aZnUrl asString
]

{ #category : #'instance creation' }
MCBitbucketRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createBitbucketRepository: aRepositorySpec
]

{ #category : #private }
MCBitbucketRepository class >> defaultHostname [
	^ 'bitbucket.org'
]

{ #category : #testing }
MCBitbucketRepository class >> isAvailableFor: type [
	^ type = 'bitbucket'
]

{ #category : #private }
MCBitbucketRepository class >> projectZipUrlFor: projectPath versionString: versionString [
  ^ 'https://bitbucket.org/' , projectPath , '/get/' , versionString , '.zip'
]

{ #category : #accessing }
MCBitbucketRepository class >> urlSchemes [
	^ #(bitbucket)
]

{ #category : #private }
MCBitbucketRepository >> normalizeTagsData: jsonObject [
  "return a dictionay mapping the tag name to the commit SHA"

  | tagDict |
  jsonObject
    at: 'error'
    ifPresent: [ :errorObject | 
      self
        error:
          'Error accessing tags for bitbucket project: ' , self projectPath printString
            , ' -> ' , (errorObject at: 'message') printString
            ,
              ((errorObject at: 'detail')
                ifNil: [ '' ]
                ifNotNil: [ :detail | ' :: ' , detail printString ]) ].
  tagDict := Dictionary new.
  jsonObject
    keysAndValuesDo: [ :tag :tagObject | 
      | sha |
      sha := tagObject at: 'node'.
      tagDict at: tag put: sha ].
  ^ tagDict
]

{ #category : #private }
MCBitbucketRepository >> projectTagsUrlFor: projectPath2 [
  ^ 'https://bitbucket.org/api/1.0/repositories/' , projectPath2 , '/tags'
]
