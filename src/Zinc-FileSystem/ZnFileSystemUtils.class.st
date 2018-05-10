"
I am ZnFileSystemUtils.
I help is abstracting over API differences pre and post the introduction of FileSystem in Pharo 2.0

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnFileSystemUtils,
	#superclass : #Object,
	#category : #'Zinc-FileSystem'
}

{ #category : #paths }
ZnFileSystemUtils class >> baseNameFor: fileName [
	^ fileName asFileReference basename 
]

{ #category : #streams }
ZnFileSystemUtils class >> binaryFileStreamFor: fileName [
	| fileReference |
	fileReference := fileName asFileReference.
	^ (fileReference respondsTo: #binaryWriteStream)
		ifTrue: [ fileReference binaryWriteStream ]
		ifFalse: [ fileReference writeStream binary ]
]

{ #category : #defaults }
ZnFileSystemUtils class >> defaultDirectory [
	^ FileSystem disk workingDirectory
]

{ #category : #defaults }
ZnFileSystemUtils class >> defaultDirectoryPath [
	^ self defaultDirectory fullName
]

{ #category : #paths }
ZnFileSystemUtils class >> delete: path [
	^ path asFileReference delete
]

{ #category : #paths }
ZnFileSystemUtils class >> deleteIfExists: path [
	^ (self exists: path)
		ifTrue: [ self delete: path. true ]
		ifFalse: [ false ]
]

{ #category : #paths }
ZnFileSystemUtils class >> directory: path [
	^ path asFileReference
]

{ #category : #paths }
ZnFileSystemUtils class >> downloadPathTo: directoryOrFileName for: baseName [
	| downloadPath |
	downloadPath := directoryOrFileName asFileReference.
	^ downloadPath isDirectory
		ifTrue: [
			baseName ifNil: [ 
				self error: 'Not enough information to construct a download path' ]. 
			(downloadPath / baseName) fullName ]
		ifFalse: [ 
			directoryOrFileName ]
	
]

{ #category : #paths }
ZnFileSystemUtils class >> exists: path [
	^ path asFileReference exists
]

{ #category : #paths }
ZnFileSystemUtils class >> extensionFor: fileName [
	^ fileName asFileReference extension
]

{ #category : #streams }
ZnFileSystemUtils class >> fileNamed: fileName do: block [
	^ fileName asFileReference 
		writeStreamDo: block
]

{ #category : #queries }
ZnFileSystemUtils class >> fileSizeFor: fileName [
	^ fileName asFileReference entry size
]

{ #category : #streams }
ZnFileSystemUtils class >> fileStreamFor: fileName [
	^ fileName asFileReference 
		writeStream
]

{ #category : #paths }
ZnFileSystemUtils class >> fullNameFor: fileName [
	^ fileName asFileReference fullName
]

{ #category : #queries }
ZnFileSystemUtils class >> modificationTimeFor: path [
	^ path asFileReference entry modificationTime 
]

{ #category : #streams }
ZnFileSystemUtils class >> newBinaryFileNamed: fileName do: block [
	| fileReference |
	fileReference := fileName asFileReference.
	^ (fileReference respondsTo: #binaryWriteStreamDo:ifPresent:)
		ifTrue: [
			fileName asFileReference 
				binaryWriteStreamDo: block 
				ifPresent: [ FileExists signalWith: fileReference ] ]
		ifFalse: [ 
			fileReference isFile 
				ifTrue: [ FileExists signalWith: fileReference ] 
				ifFalse: [ block value: (self binaryFileStreamFor: fileName) ] ]
]

{ #category : #streams }
ZnFileSystemUtils class >> newFileNamed: fileName do: block [
	^ fileName asFileReference 
		writeStreamDo: block 
		ifPresent: [ FileExists signalWith: fileName asFileReference ]
]

{ #category : #streams }
ZnFileSystemUtils class >> oldFileNamed: fileName do: block [
	^ fileName asFileReference 
		readStreamDo: block
		ifAbsent: [ FileDoesNotExist signalWith: fileName asFileReference ]
]

{ #category : #streams }
ZnFileSystemUtils class >> oldFileStreamFor: fileName [
	^ fileName asFileReference 
		readStreamIfAbsent: [ FileDoesNotExist signalWith: fileName asFileReference ]
]
