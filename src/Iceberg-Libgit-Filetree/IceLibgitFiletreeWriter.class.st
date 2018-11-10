"
I'm a filetree writer to ensure the output will be libgit2 compatible.
I'm not surre I cannot unify myself class with my parent.
"
Class {
	#name : #IceLibgitFiletreeWriter,
	#superclass : #IceMetadatalessFileTreeWriter,
	#traits : 'TIceRepositoryWriter',
	#classTraits : 'TIceRepositoryWriter classTrait',
	#category : #'Iceberg-Libgit-Filetree'
}

{ #category : #accessing }
IceLibgitFiletreeWriter class >> description [
	^ 'Filetree'
]

{ #category : #accessing }
IceLibgitFiletreeWriter class >> directoryNameFor: aPackage [
	^ aPackage name, '.package'
]

{ #category : #accessing }
IceLibgitFiletreeWriter class >> forInternalStore [
	^ IceMetadatalessFileTreeWriter
]

{ #category : #writing }
IceLibgitFiletreeWriter class >> forInternalStoreFileOut: aMCVersion on: aRepository [
	self forInternalStore 
		fileOut: aMCVersion
		on: (IceBackendWriterAdapter for: aRepository)
]

{ #category : #accessing }
IceLibgitFiletreeWriter class >> id [
	^ #filetree
]

{ #category : #testing }
IceLibgitFiletreeWriter class >> isLegacyDefault [
	^ true
]

{ #category : #testing }
IceLibgitFiletreeWriter class >> isValidPackage: aFileReference [
	^ (aFileReference basename endsWith: '.package') 
		and: [ aFileReference isDirectory 
			and: [(aFileReference / 'monticello.meta') exists]]
]

{ #category : #accessing }
IceLibgitFiletreeWriter class >> packageNameFromDirectory: aFileReference [
	^ aFileReference basenameWithoutExtension
]

{ #category : #writing }
IceLibgitFiletreeWriter >> addString: string at: fileNameOrPath encodedTo: ignored [
    "fileNameOrPath may have one or two elements"
    "encodeTo: arg, because FileTree uses UTF8 by default"

	| split path fileName |
	split := fileNameOrPath lastIndexOf: $/.
	path := fileNameOrPath first: split.
	fileName := fileNameOrPath copyFrom: split + 1.
	
	(self packageFileDirectory entryByPath: path)
		addEntryNamed: fileName withContents: string.

]

{ #category : #accessing }
IceLibgitFiletreeWriter >> fileUtils [
	^ IceLibgitFiletreeFileUtils
]

{ #category : #accessing }
IceLibgitFiletreeWriter >> snapshotWriterClass [
	^ IceLibgitFiletreeSnapshotWriter
]

{ #category : #visiting }
IceLibgitFiletreeWriter >> subPackageFileDirectoryFor: directoryNameOrPath [
	^ self packageFileDirectory entryByPath: directoryNameOrPath 
]

{ #category : #visiting }
IceLibgitFiletreeWriter >> writeVersion: aVersion [
	directory := aVersion package name, '.package'.
	packageFileDirectory := stream / directory.

	self packageFileDirectory ensureDeleteAll.
	self writeFormat.
	self writePackage: aVersion package.
	self writeDefinitions: aVersion.
	aVersion dependencies do: [ :ea | self writeVersionDependency: ea ]
]
