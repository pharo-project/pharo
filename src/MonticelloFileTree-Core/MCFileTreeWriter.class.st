"
Writer for the file tree
"
Class {
	#name : 'MCFileTreeWriter',
	#superclass : 'MCMczWriter',
	#instVars : [
		'directory',
		'packageFileDirectory',
		'cachedPackageFileDirectoryEntries'
	],
	#category : 'MonticelloFileTree-Core',
	#package : 'MonticelloFileTree-Core'
}

{ #category : 'accessing' }
MCFileTreeWriter class >> readerClass [
    "don't expect this message to be sent"

    self shouldNotImplement
]

{ #category : 'writing' }
MCFileTreeWriter >> addString: string at: fileNameOrPath encodedTo: ignored [
	"fileNameOrPath may have one or two elements"

	"encodeTo: arg, because FileTree uses UTF8 by default"

	| fullPath path |
	path := fullPath := self fileUtils buildPathFrom: {
			                    MCFileTreeStCypressWriter monticelloMetaDirName.
			                    fileNameOrPath }.
	self fileUtils ensureFilePathExists: fullPath relativeTo: self packageFileDirectory.
	fullPath.
	string isEmpty ifFalse: [ self fileUtils writeStreamFor: path in: self packageFileDirectory do: [ :file | file nextPutAll: string ] ]
]

{ #category : 'visiting' }
MCFileTreeWriter >> deleteExistingPackageStructureFor: members [

	(self fileUtils directoryExists: self packageFileDirectory) ifTrue: [ self fileUtils deleteAll: self packageFileDirectory ]
]

{ #category : 'accessing' }
MCFileTreeWriter >> fileUtils [

	^ MCFileTreeFileUtils
]

{ #category : 'writing' }
MCFileTreeWriter >> flush [
	
]

{ #category : 'visiting' }
MCFileTreeWriter >> initializePackageFileDirectoryCache [
    cachedPackageFileDirectoryEntries := Dictionary new.
    self packageFileDirectory entries
        do: [ :entry | 
            entry isDirectory
                ifTrue: [ cachedPackageFileDirectoryEntries at: entry name put: (self fileUtils directoryFromEntry: entry) ] ]
]

{ #category : 'writing' }
MCFileTreeWriter >> packageFileDirectory [
    packageFileDirectory
        ifNil: [ packageFileDirectory := self fileUtils directoryFromPath: directory relativeTo: stream directory ].
    ^ packageFileDirectory
]

{ #category : 'writing' }
MCFileTreeWriter >> repository [
  ^ stream
]

{ #category : 'accessing' }
MCFileTreeWriter >> snapshotWriterClass [

	^ MCFileTreeStCypressWriter
]

{ #category : 'visiting' }
MCFileTreeWriter >> subPackageFileDirectoryFor: directoryNameOrPath [
    ^ cachedPackageFileDirectoryEntries
        at: directoryNameOrPath
        ifAbsentPut: [ 
            | dir |
            dir := self fileUtils directoryFromPath: directoryNameOrPath relativeTo: self packageFileDirectory.
            self fileUtils ensureDirectoryExists: dir.
            dir ]
]

{ #category : 'visiting' }
MCFileTreeWriter >> writeSnapshot: aSnapshot [
	(self snapshotWriterClass on: self) writeDefinitions: aSnapshot definitions
]

{ #category : 'visiting' }
MCFileTreeWriter >> writeVersion: aVersion [
    | members |
    directory := (members := MCFileTreeRepository parseName: aVersion info name) last.
    self deleteExistingPackageStructureFor: members.
    self fileUtils ensureDirectoryExists: self packageFileDirectory.
    self initializePackageFileDirectoryCache.
    super writeVersion: aVersion
]
