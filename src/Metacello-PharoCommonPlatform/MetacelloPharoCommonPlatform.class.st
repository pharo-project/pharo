"
MetacelloPharoPlatform contains all Pharo specific code to load Metacello.
"
Class {
	#name : #MetacelloPharoCommonPlatform,
	#superclass : #MetacelloPlatform,
	#category : #'Metacello-PharoCommonPlatform'
}

{ #category : #'class initialization' }
MetacelloPharoCommonPlatform class >> initialize [
  self select
]

{ #category : #utilities }
MetacelloPharoCommonPlatform >> authorName [
	^ Author fullName
]

{ #category : #utilities }
MetacelloPharoCommonPlatform >> authorName: aString [

	Author fullName: aString
]

{ #category : #'github/bitbucket support' }
MetacelloPharoCommonPlatform >> closeZipArchive: anArchive [
	
	anArchive close
]

{ #category : #notification }
MetacelloPharoCommonPlatform >> collection: aCollection do: aBlock displaying: aString [

	self bypassProgressBars ifTrue: [ ^super collection: aCollection do: aBlock displaying: aString ].
	aCollection 
		do: aBlock
		displayingProgress: aString
]

{ #category : #utilities }
MetacelloPharoCommonPlatform >> compiler [
	^ self class compiler
]

{ #category : #reflection }
MetacelloPharoCommonPlatform >> copyClass: oldClass as: newName inCategory: newCategoryName [

	| copysName class newDefinition |
	copysName := newName asSymbol.
	copysName = oldClass name ifTrue: [ ^ oldClass ].
	(Smalltalk globals includesKey: copysName) ifTrue: [ ^ self error: copysName , ' already exists' ].
	newDefinition := oldClass definition copyReplaceAll: '#' , oldClass name asString with: '#' , copysName asString printString.
	newDefinition := newDefinition
		                 copyReplaceAll: 'category: ' , (self packageOrganizer categoryOfBehavior: oldClass) asString printString
		                 with: 'category: ' , newCategoryName printString.
	class := self compiler
		         logged: true;
		         evaluate: newDefinition.
	class class instanceVariableNames: oldClass class instanceVariablesString.
	class copyAllMethodsFrom: oldClass.
	class class copyAllMethodsFrom: oldClass class.
	class category: newCategoryName.
	^ class
]

{ #category : #'repository creation' }
MetacelloPharoCommonPlatform >> createFiletreeRepository: aRepositorySpec [
	MCRepository 
		findFiletreeAlternateFormat: aRepositorySpec
		ifFound: [ :repository | 
			^ repository createRepositoryFromSpec: aRepositorySpec on: self ].

	^ super createFiletreeRepository: aRepositorySpec
]

{ #category : #'repository creation' }
MetacelloPharoCommonPlatform >> createFtpRepository: aRepositorySpec [
 	| description headerSize index host directory |
	
	description := aRepositorySpec description.
	headerSize := 'ftp://' size.
	index := description indexOf: $/ startingAt: headerSize + 1.
	host := description copyFrom: headerSize + 1 to: index - 1.
	directory := description copyFrom: index + 1 to: description size.
	
	^ MCFtpRepository
		host: host 
		directory: directory 
		user: aRepositorySpec username
		password: aRepositorySpec password
]

{ #category : #'file system' }
MetacelloPharoCommonPlatform >> defaultDirectory [
	"Get the image default directory"
	
	^FileLocator imageDirectory asFileReference
]

{ #category : #attributes }
MetacelloPharoCommonPlatform >> defaultPlatformAttributes [
	| attributes versionString |
	((Smalltalk respondsTo: #image) and: [ Smalltalk image respondsTo: #metacelloPlatformAttributes ])
		ifTrue: [ ^ Smalltalk image metacelloPlatformAttributes ].
	attributes := OrderedCollection with: #squeakCommon with: #pharo.
	Smalltalk
		at: #SystemVersion
		ifPresent: [ :cl | 
			versionString := cl current version.
			(((versionString beginsWith: 'Pharo-1') or: [ versionString beginsWith: 'PharoCore1' ])
				or: [ versionString beginsWith: 'Pharo1' ])
				ifTrue: [ 
					attributes add: #'pharo1.x'.
					((versionString beginsWith: 'Pharo-1.0') or: [ versionString beginsWith: 'PharoCore1.0' ])
						ifTrue: [ attributes add: #'pharo1.0.x' ]
						ifFalse: [ 
							((versionString beginsWith: 'Pharo-1.1') or: [ versionString beginsWith: 'Pharo1.1' ])
								ifTrue: [ attributes add: #'pharo1.1.x' ]
								ifFalse: [ 
									((versionString beginsWith: 'Pharo-1.2') or: [ versionString beginsWith: 'Pharo1.2' ])
										ifTrue: [ attributes add: #'pharo1.2.x' ]
										ifFalse: [ 
											(versionString beginsWith: 'Pharo1.3')
												ifTrue: [ attributes add: #'pharo1.3.x' ] ] ] ] ] ].
	^ attributes
]

{ #category : #scripting }
MetacelloPharoCommonPlatform >> defaultRepositoryDescription [
  ^ 'http://smalltalkhub.com/mc/Pharo/MetaRepoForPharo30/main'
]

{ #category : #'file system' }
MetacelloPharoCommonPlatform >> deleteFileNamed: filePath [
	filePath asFileReference delete
]

{ #category : #'file system' }
MetacelloPharoCommonPlatform >> directoryFromPath: adirectoryPath relativeTo: anotherDirectoryPath [
	"Get a handle on the following path: anotherDirectoryPath/adirectoryPath"
	^ anotherDirectoryPath resolveString: adirectoryPath
]

{ #category : #notification }
MetacelloPharoCommonPlatform >> do: aBlock displaying: aString [

    self bypassProgressBars ifTrue: [ ^ super do: aBlock displaying: aString ].
	
    aString
        displayProgressFrom: 0 to: 2
        during: [:bar |
            bar value: 1.
            aBlock value.
            bar value: 2 ]
]

{ #category : #'github/bitbucket support' }
MetacelloPharoCommonPlatform >> downloadJSON: url username: username pass: pass [
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  | client json |
  client := ZnClient new
    url: url;
    yourself.
  username ifNotNil: [ client username: username password: pass ].
  client get.
  json := client contents.
  ^ STON fromString: json
]

{ #category : #'github/bitbucket support' }
MetacelloPharoCommonPlatform >> downloadZipArchive: url to: outputFileName [
	"download zip archive from <url> into <outputFileName>"

	outputFileName asFileReference ensureDelete.
	[ :bar | 
	bar title: 'Download: ' , url asString , ' to ' , outputFileName.
	[ ZnClient new
		url: url;
		signalProgress: true;
		downloadTo: outputFileName ]
		on: HTTPProgress
		do: [ :progress | 
			progress isEmpty
				ifFalse: [ bar current: progress percentage ].
			progress resume ] ] asJob run.
	^ ZipArchive new readFrom: outputFileName asFileReference
]

{ #category : #'file system' }
MetacelloPharoCommonPlatform >> ensureDirectoryExists: aDirectoryHandle [
	"Ensure the directory exists."

	^ aDirectoryHandle ensureCreateDirectory ; yourself
]

{ #category : #'github/bitbucket support' }
MetacelloPharoCommonPlatform >> extractRepositoryFrom: zipFile to: directory [
    "unzip <zipFile> into <directory>"
	ZipArchive new 
		readFrom:  zipFile;
		extractAllTo: directory asFileReference.
	zipFile asFileReference delete.
]

{ #category : #'github/bitbucket support' }
MetacelloPharoCommonPlatform >> fileDirectoryClass [
	^FileSystem
]

{ #category : #'file system' }
MetacelloPharoCommonPlatform >> fileHandleOn: aPath [
	"Get an handle on a file."

	^ aPath asFileReference
]

{ #category : #monticello }
MetacelloPharoCommonPlatform >> newVersionForWorkingCopy: aWorkingCopy [
	^aWorkingCopy newVersionIn: aWorkingCopy repositoryGroup
]

{ #category : #'repository creation' }
MetacelloPharoCommonPlatform >> packageInfoFor: aMCWorkingCopy [
  "MCPackageManager>>packageInfo is deprecated in Pharo2.0"

  ^ aMCWorkingCopy packageSet
]

{ #category : #'file system' }
MetacelloPharoCommonPlatform >> parentDirectoryOf: aFileHandler [
	"Get the parent directory of this file."
	
	^ aFileHandler parent
]

{ #category : #'file system' }
MetacelloPharoCommonPlatform >> recursiveDelete: aDirectory [
	"delete this directory and all children of it"

	^  aDirectory deleteAll
]

{ #category : #'repository creation' }
MetacelloPharoCommonPlatform >> registerPackageNamed: packageName [
  "PackageInfo deprecated in Pharo3.0"

  ^RPackageSet named: packageName
]

{ #category : #system }
MetacelloPharoCommonPlatform >> selectorForPragma: aPragma [
	^ aPragma methodSelector
]

{ #category : #utilities }
MetacelloPharoCommonPlatform >> string: aString includesSubstring: aSubstring [
	"abstract String>>includesSubstring: to have the same message on all supported platforms."

	^aString includesSubstring: aSubstring
]

{ #category : #system }
MetacelloPharoCommonPlatform >> suspendSystemUpdateEventsDuring: aBlock [
	"Wraps call to the system evetn manager to tell it that we want to suspend events during execution of aBlock"
	"From Pharo2, we cannot suspend system events since missing events brak the image (rpackage at least should be notified."
	"SystemAnnouncer uniqueInstance suspendAllWhile: aBlock"
	^ aBlock value
]

{ #category : #'file system' }
MetacelloPharoCommonPlatform >> tempFileFor: aName suffix: aSuffixString [
	^ (FileLocator temp asFileReference
		/ (FileReference newTempFilePrefix: aName suffix: aSuffixString) basename)
		fullName
]

{ #category : #utilities }
MetacelloPharoCommonPlatform >> timestamp [

	^Date today mmddyyyy, ' ',
		((String streamContents: [:s | Time now print24: true on: s]) copyFrom: 1 to: 5)
]
