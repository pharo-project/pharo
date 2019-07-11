"
I am a file handle on a read-only store.
"
Class {
	#name : #ReadOnlyFileHandle,
	#superclass : #FileHandle,
	#category : #'FileSystem-Disk-Base'
}

{ #category : #testing }
ReadOnlyFileHandle >> isWritable [
	^ false
]

{ #category : #testing }
ReadOnlyFileHandle >> setReference: aReference writable: aBoolean [

	reference := aReference resolve.

	aBoolean ifTrue: [ 
		ReadOnlyFileException signal: 'Attempt to open file ', reference pathString, ' as writable on a read-only file system' ].

	writable := aBoolean
]

{ #category : #testing }
ReadOnlyFileHandle >> truncateTo: anInteger [

	ReadOnlyFileException signal: 'Attempt to truncate file ', reference pathString, ' on a read-only file system'.

	^ self
]
