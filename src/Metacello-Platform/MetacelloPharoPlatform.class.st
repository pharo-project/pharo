"
Class used to abstract specific code for the Pharo platform.

"
Class {
	#name : #MetacelloPharoPlatform,
	#superclass : #MetacelloPharoCommonPlatform,
	#category : #'Metacello-Platform'
}

{ #category : #'initialize-release' }
MetacelloPharoPlatform class >> initialize [
  self select


]

{ #category : #utilities }
MetacelloPharoPlatform >> compiler [
	^ self class compiler
]

{ #category : #'file system' }
MetacelloPharoPlatform >> defaultDirectory [
	"Get the image default directory"
	
	^FileLocator imageDirectory asFileReference
]

{ #category : #scripting }
MetacelloPharoPlatform >> defaultRepositoryDescription [
  ^ 'http://smalltalkhub.com/mc/Pharo/MetaRepoForPharo30/main'

]

{ #category : #'file system' }
MetacelloPharoPlatform >> deleteFileNamed: filePath [
	filePath asFileReference delete
]

{ #category : #'file system' }
MetacelloPharoPlatform >> directoryFromPath: adirectoryPath relativeTo: anotherDirectoryPath [
	"Get a handle on the following path: anotherDirectoryPath/adirectoryPath"
	^ anotherDirectoryPath resolveString: adirectoryPath
]

{ #category : #'github/bitbucket support' }
MetacelloPharoPlatform >> downloadJSON: url username: username pass: pass [
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

{ #category : #'github support' }
MetacelloPharoPlatform >> downloadZipArchive: url to: outputFileName [
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
MetacelloPharoPlatform >> ensureDirectoryExists: aDirectoryHandle [
	"Ensure the directory exists."

	^ aDirectoryHandle ensureCreateDirectory ; yourself
]

{ #category : #'github support' }
MetacelloPharoPlatform >> extractRepositoryFrom: zipFile to: directory [
    "unzip <zipFile> into <directory>"
	ZipArchive new 
		readFrom:  zipFile;
		extractAllTo: directory asFileReference.
	zipFile asFileReference delete.
]

{ #category : #'file system' }
MetacelloPharoPlatform >> fileDirectoryClass [
	^FileSystem
]

{ #category : #'file system' }
MetacelloPharoPlatform >> fileFromPath: aFileName relativeTo: aDirectoryPath [
	"Get a handle on the following path: anotherDirectoryPath/aFileName"
	^ aDirectoryPath / aFileName
]

{ #category : #'file system' }
MetacelloPharoPlatform >> fileHandleOn: aPath [
	"Get an handle on a file."

	^ aPath asFileReference
]

{ #category : #monticello }
MetacelloPharoPlatform >> newVersionForWorkingCopy: aWorkingCopy [
	^aWorkingCopy newVersionIn: aWorkingCopy repositoryGroup
]

{ #category : #'pharo 2.0 compat' }
MetacelloPharoPlatform >> packageInfoFor: aMCWorkingCopy [
  "MCPackageManager>>packageInfo is deprecated in Pharo2.0"

  ^ aMCWorkingCopy packageSet
]

{ #category : #'file system' }
MetacelloPharoPlatform >> parentDirectoryOf: aFileHandler [
	"Get the parent directory of this file."
	
	^ aFileHandler parent
]

{ #category : #'file system' }
MetacelloPharoPlatform >> readStreamOn: aFileHandle do: aBlock [
	"Get a read stream on the file handle and execute some actions on it."
	^ aFileHandle readStreamDo: aBlock
]

{ #category : #'file system' }
MetacelloPharoPlatform >> recursiveDelete: aDirectory [
	"delete this directory and all children of it"

	^  aDirectory deleteAll
]

{ #category : #'pharo 2.0 compat' }
MetacelloPharoPlatform >> registerPackageNamed: packageName [
  "PackageInfo deprecated in Pharo3.0"

  ^RPackageSet named: packageName

]

{ #category : #system }
MetacelloPharoPlatform >> selectorForPragma: aPragma [
	^ aPragma methodSelector
]

{ #category : #utilities }
MetacelloPharoPlatform >> string: aString includesSubstring: aSubstring [
	"abstract String>>includesSubstring: to have the same message on all supported platforms."

	^aString includesSubstring: aSubstring
]

{ #category : #system }
MetacelloPharoPlatform >> suspendSystemUpdateEventsDuring: aBlock [
	"Wraps call to the system evetn manager to tell it that we want to suspend events during execution of aBlock"
	"From Pharo2, we cannot suspend system events since missing events brak the image (rpackage at least should be notified."
	"SystemAnnouncer uniqueInstance suspendAllWhile: aBlock"
	^ aBlock value

]

{ #category : #'file system' }
MetacelloPharoPlatform >> tempFileFor: aName suffix: aSuffixString [
	^ (FileLocator temp asFileReference
		/ (FileReference newTempFilePrefix: aName suffix: aSuffixString) basename)
		fullName
]
