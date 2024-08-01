"
I am a special monticello directory-based repository that is used for the global monticello cache.

Most other repositories will first query the default cache repository for existing files before handling the request by themselves. For instance an http repository will first check if the file is in the caceh before doing a ""slow"" download.
"
Class {
	#name : 'MCCacheRepository',
	#superclass : 'MCDirectoryRepository',
	#instVars : [
		'cacheEnabled'
	],
	#classInstVars : [
		'default'
	],
	#category : 'Monticello-Repositories',
	#package : 'Monticello',
	#tag : 'Repositories'
}

{ #category : 'accessing settings' }
MCCacheRepository class >> cacheDirectory [
	^self uniqueInstance directory asAbsolute
	
]

{ #category : 'accessing settings' }
MCCacheRepository class >> cacheDirectory: directory [
	"directory - can be an absolutePathString (to interface with the settings framework), or aFileReference"
 
	default := self new directory: directory asFileReference.
]

{ #category : 'accessing' }
MCCacheRepository class >> default [
	"This should be deleted as soon as Metacello is fixed"
	self flag: #toBeDeleted.			
	^ self uniqueInstance 
]

{ #category : 'defaults' }
MCCacheRepository class >> defaultDirectory [
	| directories |
	directories := {
		((FileLocator localDirectory / 'package-cache') asFileReference).	
		(FileLocator temp).	
		(FileLocator home)}.
	directories
		do: [ :directory | 
			[ ^ directory ensureCreateDirectory; yourself ]
				on: Error do: [ "ignore and try the next directory" ] ].
	Error signal: 'Can''t create a package-cache'
]

{ #category : 'accessing' }
MCCacheRepository class >> description [
	^ nil
]

{ #category : 'utilities' }
MCCacheRepository class >> disableCacheDuring: aBlock [
	self uniqueInstance disableCacheDuring: aBlock
]

{ #category : 'class initialization' }
MCCacheRepository class >> initialize [
	self resetIfInvalid
]

{ #category : 'class initialization' }
MCCacheRepository class >> reset [

	default := nil.
]

{ #category : 'utilities' }
MCCacheRepository class >> resetIfInvalid [
	default isNotNil and: [ default directory ifAbsent: [ default := nil ] ]
]

{ #category : 'accessing' }
MCCacheRepository class >> uniqueInstance [
	self resetIfInvalid.
	^ default ifNil: [default := self new directory: self defaultDirectory]
]

{ #category : 'storing' }
MCCacheRepository >> basicStoreVersion: aVersion [
	"we store a version if it is cacheable and we don't have the file already"

	(self includesFileNamed: aVersion fileName) ifFalse: [ super basicStoreVersion: aVersion ]
]

{ #category : 'utilities' }
MCCacheRepository >> disable [
	cacheEnabled := false.
]

{ #category : 'utilities' }
MCCacheRepository >> disableCacheDuring: aBlock [
	self disable.
	aBlock ensure: [ self enable ].
]

{ #category : 'utilities' }
MCCacheRepository >> enable [
	cacheEnabled := true
]

{ #category : 'initialization' }
MCCacheRepository >> initialize [

	super initialize.
	cacheEnabled := true
]

{ #category : 'storing' }
MCCacheRepository >> storeVersion: aVersion [

	cacheEnabled ifFalse: [ ^ self ].
	^ super storeVersion: aVersion
]

{ #category : 'utilities' }
MCCacheRepository >> useDirectory: aFolder during: aBlock [

	| oldDirectory |
	oldDirectory := self directory.
	[
	self directory: aFolder.
	aBlock value ] ensure: [ self directory: oldDirectory ]
]

{ #category : 'interface' }
MCCacheRepository >> versionWithInfo: aVersionInfo ifAbsent: errorBlock [
	(self allFileNamesForVersionNamed: aVersionInfo name) do: [:fileName | | version |
			version := self versionFromFileNamed: fileName.
			version info = aVersionInfo ifTrue: [^ version]].
	^ errorBlock value
]

{ #category : 'writing' }
MCCacheRepository >> writeStreamWithReplacingForFileNamed: aString do: aBlock [

	| file |
	file := directory / aString.
	file ensureDelete.
	file binaryWriteStreamDo: aBlock
]
