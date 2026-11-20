"
I am a strategy for for choosing between the old and new directory implementation.
"
Class {
	#name : 'DirectoryAPI',
	#superclass : 'Object',
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'api' }
DirectoryAPI class >> contentsOf: path [
	^ self currentDirectoryClass contentsOf: path
]

{ #category : 'api' }
DirectoryAPI class >> create: path [
	^ self currentDirectoryClass create: path
]

{ #category : 'private' }
DirectoryAPI class >> currentDirectoryClass [
	^ self useNewDirectory ifTrue: [ NewDirectory ] ifFalse: [ OldDirectory ]
]

{ #category : 'api' }
DirectoryAPI class >> open: path [
	^ self currentDirectoryClass open: path
]

{ #category : 'api' }
DirectoryAPI class >> removeEmpty: path [
	^ self currentDirectoryClass removeEmpty: path
]

{ #category : 'private' }
DirectoryAPI class >> useNewDirectory [
	^ NewFile primitiveIsAvailable
]
