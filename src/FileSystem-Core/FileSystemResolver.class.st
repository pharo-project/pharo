"
I am an abstract superclass for objects that can resolve origins into references. Such objects use the Chain of Responsibility pattern, and when unable to resolve a particular origin, delegate that resolution request to the next resolver in the list.

next
	The next resolver in the list, or nil

"
Class {
	#name : #FileSystemResolver,
	#superclass : #Object,
	#instVars : [
		'next'
	],
	#category : #'FileSystem-Core-Base-Resolver'
}

{ #category : #accessing }
FileSystemResolver >> addResolver: aResolver [
	next
		ifNil: [next := aResolver]
		ifNotNil: [next addResolver: aResolver]
]

{ #category : #resolving }
FileSystemResolver >> canResolve: aSymbol [
	^ self supportedOrigins includes: aSymbol
]

{ #category : #accessing }
FileSystemResolver >> flushCaches [
	self flushLocalCache.
	next ifNotNil: [next flushCaches]
]

{ #category : #accessing }
FileSystemResolver >> flushLocalCache [
]

{ #category : #accessing }
FileSystemResolver >> next [
	^ next
]

{ #category : #resolving }
FileSystemResolver >> resolve: aSymbol [
	^ (self canResolve: aSymbol)
		ifTrue: [self perform: aSymbol]
		ifFalse: [self unknownOrigin: aSymbol]
]

{ #category : #resolving }
FileSystemResolver >> resolveString: aString [
	| decoded fs |
	"The argument string is actually a byte array encoded differently on each platform.
	We are transforming it to an image string.
	We assume for now that the string is utf8 encoded."
	decoded := aString asByteArray utf8Decoded.
	fs := FileSystem disk.
	^ FileReference 
		fileSystem: fs 
		path: (fs pathFromString: decoded)
]

{ #category : #resolving }
FileSystemResolver >> supportedOrigins [
	^ #()
]

{ #category : #resolving }
FileSystemResolver >> unknownOrigin: aSymbol [
	^ next ifNotNil: [next resolve: aSymbol]
]
