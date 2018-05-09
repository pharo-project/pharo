"
I'm a specific store for OSX file systems
"
Class {
	#name : #MacStore,
	#superclass : #UnixStore,
	#category : #'FileSystem-Disk-Store'
}

{ #category : #current }
MacStore class >> isActiveClass [
	^ Smalltalk os isMacOS
]

{ #category : #public }
MacStore >> mimeTypesAt: aPath [
	"Return a list of MIME types applicable to the receiver. This default implementation uses the file name extension to figure out what we're looking at but specific subclasses may use other means of figuring out what the type of some file is. Some systems like the macintosh use meta data on the file to indicate data type"
	"| typeCreator type | 
	typeCreator := self getMacFileTypeAndCreator: ((self fullNameFor: fileName)).
	type := (typeCreator at: 1) asLowercase.
	^TypeToMimeMappings at: type ifAbsent:[super mimeTypesFor: fileName]"
	self flag: 'TODO: properly implement the mac os version'.
	^ super mimeTypesAt: aPath
]
