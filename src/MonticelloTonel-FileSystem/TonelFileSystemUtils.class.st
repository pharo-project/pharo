"
I'm a file utils implementation who works with FileSystem framework.
Pharo should use me.
"
Class {
	#name : #TonelFileSystemUtils,
	#superclass : #TonelFileUtils,
	#category : #'MonticelloTonel-FileSystem'
}

{ #category : #utilities }
TonelFileSystemUtils class >> default [
    ^ DiskStore activeClass createDefault defaultWorkingDirectory asFileReference
]

{ #category : #utilities }
TonelFileSystemUtils class >> deleteAll: aDirectory [
    ^ aDirectory deleteAll
]

{ #category : #utilities }
TonelFileSystemUtils class >> directoryExists: aDirectory [
    ^ aDirectory isDirectory
]

{ #category : #utilities }
TonelFileSystemUtils class >> directoryFromEntry: directoryEntry [
    ^ directoryEntry asFileReference
]

{ #category : #utilities }
TonelFileSystemUtils class >> directoryFromPath: directoryPath [
    ^ (AbsolutePath from: directoryPath delimiter: self pathNameDelimiter) asFileReference
]

{ #category : #utilities }
TonelFileSystemUtils class >> directoryFromPath: directoryPath relativeTo: aDirectory [
    ^ aDirectory resolveString: directoryPath
]

{ #category : #utilities }
TonelFileSystemUtils class >> directoryName: aDirectory [
    ^ aDirectory basename
]

{ #category : #utilities }
TonelFileSystemUtils class >> directoryPathString: aDirectory [
    ^ aDirectory pathString
]

{ #category : #utilities }
TonelFileSystemUtils class >> ensureDirectoryExists: aDirectory [
    aDirectory ensureCreateDirectory
]

{ #category : #utilities }
TonelFileSystemUtils class >> ensureFilePathExists: fileNameOrPath relativeTo: aDirectory [
    (aDirectory resolveString: fileNameOrPath) parent ensureCreateDirectory
]

{ #category : #utilities }
TonelFileSystemUtils class >> fileName: aDirectory [
    ^ aDirectory basename
]

{ #category : #utilities }
TonelFileSystemUtils class >> filePathExists: filePath relativeTo: aDirectory [
    ^ (aDirectory resolveString: filePath) isFile
]

{ #category : #initialization }
TonelFileSystemUtils class >> initialize [
    "self initialize"

    self install
]

{ #category : #utilities }
TonelFileSystemUtils class >> parentDirectoryOf: aDirectory [
    ^ aDirectory parent
]

{ #category : #utilities }
TonelFileSystemUtils class >> pathNameDelimiter [
    ^ DiskStore activeClass delimiter
]

{ #category : #utilities }
TonelFileSystemUtils class >> readStreamFor: filePath do: aBlock [
    ^ filePath asFileReference
        readStreamDo: [ :fileStream | 
            | stream |
            stream := ReadStream on: fileStream contents asString.
            stream reset.
            aBlock value: stream ]
]

{ #category : #utilities }
TonelFileSystemUtils class >> readStreamFor: filePath in: aDirectory do: aBlock [
    "temporary hack for Pharo-1.4"

    ^ (aDirectory resolveString: filePath)
        readStreamDo: [ :fileStream | 
            | stream |
            stream := ReadStream on: fileStream contents asString.
            stream reset.
            aBlock value: stream ]
]

{ #category : #utilities }
TonelFileSystemUtils class >> resolvePath: path in: aDirectory [
    ^ aDirectory resolveString: path
]

{ #category : #utilities }
TonelFileSystemUtils class >> writeStreamFor: filePath in: aDirectory do: aBlock [
    (aDirectory resolveString: filePath) 
		ensureDelete;
		writeStreamDo: aBlock
]
