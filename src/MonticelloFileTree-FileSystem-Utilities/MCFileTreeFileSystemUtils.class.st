Class {
	#name : #MCFileTreeFileSystemUtils,
	#superclass : #MCFileTreeFileUtils,
	#category : #'MonticelloFileTree-FileSystem-Utilities'
}

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> default [
    ^ DiskStore activeClass createDefault defaultWorkingDirectory asFileReference
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> deleteAll: aDirectory [
    ^ aDirectory deleteAll
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> directoryExists: aDirectory [
    ^ aDirectory isDirectory
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> directoryFromEntry: directoryEntry [
    ^ directoryEntry asFileReference
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> directoryFromPath: directoryPath [
    ^ (AbsolutePath from: directoryPath delimiter: self pathNameDelimiter) asFileReference
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> directoryFromPath: directoryPath relativeTo: aDirectory [
    ^ aDirectory resolveString: directoryPath
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> directoryName: aDirectory [
    ^ aDirectory basename
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> directoryPathString: aDirectory [
    ^ aDirectory pathString
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> ensureDirectoryExists: aDirectory [
    aDirectory ensureCreateDirectory
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> ensureFilePathExists: fileNameOrPath relativeTo: aDirectory [
    (aDirectory resolveString: fileNameOrPath) parent ensureCreateDirectory
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> filePathExists: filePath relativeTo: aDirectory [
    ^ (aDirectory resolveString: filePath) isFile
]

{ #category : #initialization }
MCFileTreeFileSystemUtils class >> initialize [
    "self initialize"

    self install
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> parentDirectoryOf: aDirectory [
    ^ aDirectory parent
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> pathNameDelimiter [
    ^ DiskStore activeClass delimiter
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> readStreamFor: filePath in: aDirectory do: aBlock [
    "temporary hack for Pharo-1.4"

    ^ (aDirectory resolveString: filePath)
        readStreamDo: [ :fileStream | 
            | stream |
            stream := ReadStream on: fileStream contents asString.
            stream reset.
            aBlock value: stream ]
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> resolvePath: path in: aDirectory [
    ^ aDirectory resolveString: path
]

{ #category : #utilities }
MCFileTreeFileSystemUtils class >> writeStreamFor: filePath in: aDirectory do: aBlock [
    (aDirectory resolveString: filePath) writeStreamDo: aBlock
]
