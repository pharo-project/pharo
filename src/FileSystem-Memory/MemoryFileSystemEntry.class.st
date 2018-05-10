"
I am an abstract file system entry for a memory file system.
My subclasses should specialize on the kind of file they are.
"
Class {
	#name : #MemoryFileSystemEntry,
	#superclass : #Object,
	#instVars : [
		'creationTime',
		'modificationTime',
		'basename'
	],
	#category : #'FileSystem-Memory-Base'
}

{ #category : #'as yet unclassified' }
MemoryFileSystemEntry class >> named: aFileName [
	^ self new
		basename: aFileName;
		yourself
]

{ #category : #accessing }
MemoryFileSystemEntry >> basename [
	^ basename
]

{ #category : #accessing }
MemoryFileSystemEntry >> basename: aString [
	basename := aString
]

{ #category : #accessing }
MemoryFileSystemEntry >> creationTime [
	
	^ creationTime
]

{ #category : #accessing }
MemoryFileSystemEntry >> fileSize [
	self subclassResponsibility 
]

{ #category : #initialization }
MemoryFileSystemEntry >> initialize [ 
	creationTime := modificationTime := DateAndTime now.
	super initialize
]

{ #category : #testing }
MemoryFileSystemEntry >> isDirectory [
	self subclassResponsibility
]

{ #category : #testing }
MemoryFileSystemEntry >> isFile [
	^ self isDirectory not
]

{ #category : #accessing }
MemoryFileSystemEntry >> modificationTime [
	
	^ modificationTime
]

{ #category : #accessing }
MemoryFileSystemEntry >> modificationTime: anObject [
	
	modificationTime := anObject
]

{ #category : #private }
MemoryFileSystemEntry >> modified [
	modificationTime := DateAndTime now.
]
