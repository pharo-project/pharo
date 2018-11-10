"
I declare file operations to be used in tonel. 
I'm necesary because tonel is meant to be portable, and different dialects have different ways of dealing with file systems. 
"
Class {
	#name : #TonelFileUtils,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : #'MonticelloTonel-Core'
}

{ #category : #utilities }
TonelFileUtils class >> buildPathFrom: pathCollection [
    ^ String
        streamContents: [ :stream | pathCollection do: [ :element | stream nextPutAll: element ] separatedBy: [ stream nextPut: self pathNameDelimiter ] ]
]

{ #category : #accessing }
TonelFileUtils class >> current [
    ^ Current
]

{ #category : #utilities }
TonelFileUtils class >> default [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> deleteAll: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> directoryExists: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> directoryFromPath: directoryPath [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> directoryFromPath: directoryPath relativeTo: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> directoryName: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> directoryPathString: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> ensureDirectoryExists: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> ensureFilePathExists: fileNameOrPath relativeTo: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> fileName: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> filePathExists: filePath relativeTo: aDirectory [
    self subclassResponsibility
]

{ #category : #initialization }
TonelFileUtils class >> install [
    Current := self
]

{ #category : #utilities }
TonelFileUtils class >> parentDirectoryOf: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> pathNameDelimiter [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> readStreamFor: filePath do: aBlock [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> readStreamFor: filePath in: aDirectory do: aBlock [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> resolvePath: path in: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
TonelFileUtils class >> writeStreamFor: filePath in: aDirectory do: aBlock [
    self subclassResponsibility
]
