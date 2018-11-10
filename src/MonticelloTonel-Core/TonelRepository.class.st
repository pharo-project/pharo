"
I'm a tonel monticello repository
"
Class {
	#name : #TonelRepository,
	#superclass : #MCDirectoryRepository,
	#category : #'MonticelloTonel-Core'
}

{ #category : #'instance creation' }
TonelRepository class >> basicFromUrl: anUrl [
	"remove 'tonel://' and go"
	^ self new directory: (anUrl asString allButFirst: self description size) asFileReference
]

{ #category : #'metacello support' }
TonelRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createTonelRepository: aRepositorySpec
]

{ #category : #accessing }
TonelRepository class >> description [
    ^ 'tonel://'
]

{ #category : #'metacello support' }
TonelRepository class >> isAvailableFor: type [
	^ type = 'tonel'
]

{ #category : #'metacello support' }
TonelRepository class >> isFiletreeAlternateFormat: aRepositorySpec [
	| location propRef |
	
	location := (aRepositorySpec description allButFirst: ('filetree://' size)) asFileReference.
	propRef := location / '.properties'.
	propRef exists ifFalse: [ ^ false ].
	
	^ propRef readStreamDo:[ :stream | 
		((STONJSON fromStream: stream) at: #format ifAbsent: []) = #tonel ]
]

{ #category : #accessing }
TonelRepository class >> urlSchemes [
	^ #(tonel)
]

{ #category : #accessing }
TonelRepository >> allFileNamesForVersionNamed: aString [
	^ self 
		filterFileNames: self readableFileNames 
		forVersionNamed: aString
]

{ #category : #accessing }
TonelRepository >> asRepositorySpecFor: aMetacelloMCProject [
    ^ aMetacelloMCProject repositorySpec
        description: self description;
        type: 'tonel';
        yourself
]

{ #category : #accessing }
TonelRepository >> basicStoreVersion: aVersion [
  TonelWriter 
	fileOut: aVersion 
	on: self directory
]

{ #category : #accessing }
TonelRepository >> description [
    ^ self class description, super description
]

{ #category : #accessing }
TonelRepository >> fileDirectoryOn: directoryPath [
	^ self fileUtils 
		directoryFromPath: directoryPath 
		relativeTo: self directory
]

{ #category : #accessing }
TonelRepository >> fileUtils [
	^ TonelFileUtils current
]

{ #category : #accessing }
TonelRepository >> filterFileNames: aCollection forVersionNamed: aString [
	^ aCollection select: [:ea | (self versionNameFromFileName: ea) = aString]
]

{ #category : #'metacello support' }
TonelRepository >> goferVersionFrom: aVersionReference [
	"Simillar hack than FileTree repositories."

	((self readableFileNames collect: [ :fileName | self fileDirectoryOn: fileName ])
		select: [ :packageDirectory | self fileUtils directoryExists: packageDirectory ])
		collect: [ :packageDirectory | 
			(self versionInfoForPackageDirectory: packageDirectory) name = aVersionReference name
				ifTrue: [ ^ self loadVersionFromFileNamed: (self fileUtils directoryName: packageDirectory) ] ].
	^ nil
]

{ #category : #accessing }
TonelRepository >> loadAllFileNames [
	^ super loadAllFileNames 
		select: [ :each | 
			self fileUtils 
				filePathExists: each, '/package.st' 
				relativeTo: self directory ]
]

{ #category : #accessing }
TonelRepository >> packageDescriptionFromPackageDirectory: packageDirectory [
    | filename |
    filename := self fileUtils directoryName: packageDirectory.
    ^ {filename. 'tonel'. 1. filename}
]

{ #category : #accessing }
TonelRepository >> packageDescriptionsFromReadableFileNames [
    ^ ((self readableFileNames 
		collect: [ :each | self fileUtils directoryFromPath: each relativeTo: self directory ])
		select: [ :each | self fileUtils directoryExists: each ])
		collect: [ :each | self packageDescriptionFromPackageDirectory: each ]
]

{ #category : #actions }
TonelRepository >> readStreamForFileNamed: aString do: aBlock [
    ^ aBlock value: self directory
]

{ #category : #accessing }
TonelRepository >> readableFileNames [
	| all cached new |

	all := self allFileNames. "in disk"
	all := all reject: [ :each | each beginsWith: '.' ].
	cached := self cachedFileNames. "in memory"
	new := all \ cached.
	^ (cached asArray, new) select: [:ea | self canReadFileNamed: ea]
]

{ #category : #accessing }
TonelRepository >> readableFileReferences [
	^ self readableFileNames collect: [ :each | 
		self fileUtils 
			directoryFromPath: each 
			relativeTo: self directory ]
]

{ #category : #accessing }
TonelRepository >> versionFrom: aVersionReferenceString [
    "until we no longer find .tree directories in the wild"

   	^ self readableFileReferences
		detect: [ :each | 
			(self fileUtils directoryExists: each)
			and: [ 
				each basename = aVersionReferenceString 
				or: [ (self versionInfoForPackageDirectory: each) name = aVersionReferenceString ] ] ]
		ifFound: [ :packageDirectory | 
			self loadVersionFromFileNamed: (self fileUtils directoryName: packageDirectory) ]
		ifNone: [ 
			nil ].
]

{ #category : #versions }
TonelRepository >> versionInfoForPackageDirectory: packageDirectory [
	^ ((MCReader readerClassForFileNamed: (self fileUtils directoryName: packageDirectory))
		on: (self fileUtils parentDirectoryOf: packageDirectory)
		fileName: (self fileUtils directoryName: packageDirectory))
		loadVersionInfo;
		info
]

{ #category : #versions }
TonelRepository >> versionInfoFromVersionNamed: aString [
	^ self versionInfoForPackageDirectory: (self fileDirectoryOn: aString)
]

{ #category : #versions }
TonelRepository >> versionNameFromFileName: aString [
	| description |
	description := self packageDescriptionFromPackageDirectory: (self fileDirectoryOn: aString).
	^ description first , '-' , description second , '.' , description third printString
]

{ #category : #versions }
TonelRepository >> versionWithInfo: aVersionInfo ifAbsent: errorBlock [
	(self allFileNamesForVersionNamed: aVersionInfo name)
		ifNotEmpty: [ :aCollection | ^ self versionFromFileNamed: aCollection first ].
	^ errorBlock value
]
