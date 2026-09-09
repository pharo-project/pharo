"
Utility class for file access
"
Class {
	#name : 'MCFileTreeFileUtils',
	#superclass : 'Object',
	#category : 'MonticelloFileTree-Core',
	#package : 'MonticelloFileTree-Core'
}

{ #category : 'utilities' }
MCFileTreeFileUtils class >> buildPathFrom: pathCollection [

	^ String streamContents: [ :stream | pathCollection do: [ :element | stream nextPutAll: element ] separatedBy: [ stream nextPut: self pathNameDelimiter ] ]
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> default [
    ^ DiskStore activeClass createDefault defaultWorkingDirectory asFileReference
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> deleteAll: aDirectory [
    ^ aDirectory deleteAll
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> directoryExists: aDirectory [
    ^ aDirectory isDirectory
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> directoryFromEntry: directoryEntry [
    ^ directoryEntry asFileReference
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> directoryFromPath: directoryPath [
    ^ (AbsolutePath from: directoryPath delimiter: self pathNameDelimiter) asFileReference
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> directoryFromPath: directoryPath relativeTo: aDirectory [
    ^ aDirectory resolveString: directoryPath
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> directoryName: aDirectory [
    ^ aDirectory basename
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> directoryPathString: aDirectory [
    ^ aDirectory pathString
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> ensureDirectoryExists: aDirectory [
    aDirectory ensureCreateDirectory
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> ensureFilePathExists: fileNameOrPath relativeTo: aDirectory [
    (aDirectory resolveString: fileNameOrPath) parent ensureCreateDirectory
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> filePathExists: filePath relativeTo: aDirectory [
    ^ (aDirectory resolveString: filePath) isFile
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> parentDirectoryOf: aDirectory [
    ^ aDirectory parent
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> pathNameDelimiter [
    ^ DiskStore activeClass delimiter
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> readStreamFor: filePath in: aDirectory do: aBlock [
    "temporary hack for Pharo-1.4"

    ^ (aDirectory resolveString: filePath)
        readStreamDo: [ :fileStream | 
            | stream |
            stream := ReadStream on: fileStream contents asString.
            stream reset.
            aBlock value: stream ]
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> resolvePath: path in: aDirectory [
    ^ aDirectory resolveString: path
]

{ #category : 'utilities' }
MCFileTreeFileUtils class >> writeStreamFor: filePath in: aDirectory do: aBlock [
    (aDirectory resolveString: filePath) 
		ensureDelete;
		writeStreamDo: aBlock
]
