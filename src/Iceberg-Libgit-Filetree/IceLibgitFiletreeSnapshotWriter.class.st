"
I'm a filetree snapshot writer to ensure the output will be libgit2 compatible.
I'm not surre I cannot unify myself class with my parent.
"
Class {
	#name : #IceLibgitFiletreeSnapshotWriter,
	#superclass : #IceMetadatalessStCypressWriter,
	#category : #'Iceberg-Libgit-Filetree'
}

{ #category : #accessing }
IceLibgitFiletreeSnapshotWriter >> fileUtils [
	^ IceLibgitFiletreeFileUtils
]

{ #category : #'initialize-release' }
IceLibgitFiletreeSnapshotWriter >> propertyFileExtension [
  ^ MCFileTreeRepository defaultPropertyFileExtension
]

{ #category : #visiting }
IceLibgitFiletreeSnapshotWriter >> writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock [
    | directory |
    directory := self directoryForDirectoryNamed: directoryNameOrPath.
    self fileUtils
        writeStreamFor: fileName , ext
        in: directory
        do: [ :fs | 
            self setFileStream: fs.
            visitBlock value ]
]
