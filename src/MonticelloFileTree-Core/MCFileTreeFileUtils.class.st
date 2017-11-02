"
Utility class for file access
"
Class {
	#name : #MCFileTreeFileUtils,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : 'MonticelloFileTree-Core'
}

{ #category : #utilities }
MCFileTreeFileUtils class >> buildPathFrom: pathCollection [
    ^ String
        streamContents: [ :stream | pathCollection do: [ :element | stream nextPutAll: element ] separatedBy: [ stream nextPut: self pathNameDelimiter ] ]
]

{ #category : #accessing }
MCFileTreeFileUtils class >> current [
    ^ Current
]

{ #category : #utilities }
MCFileTreeFileUtils class >> default [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> deleteAll: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> directoryExists: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> directoryFromPath: directoryPath [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> directoryFromPath: directoryPath relativeTo: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> directoryName: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> directoryPathString: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> ensureDirectoryExists: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> ensureFilePathExists: fileNameOrPath relativeTo: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> filePathExists: filePath relativeTo: aDirectory [
    self subclassResponsibility
]

{ #category : #initialization }
MCFileTreeFileUtils class >> install [
    Current := self
]

{ #category : #utilities }
MCFileTreeFileUtils class >> parentDirectoryOf: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> pathNameDelimiter [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> readStreamFor: filePath in: aDirectory do: aBlock [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> resolvePath: path in: aDirectory [
    self subclassResponsibility
]

{ #category : #utilities }
MCFileTreeFileUtils class >> writeStreamFor: filePath in: aDirectory do: aBlock [
    self subclassResponsibility
]
