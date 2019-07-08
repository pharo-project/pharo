Class {
	#name : #ReadOnlyUnixStore,
	#superclass : #UnixStore,
	#category : #'FileSystem-Disk-Store'
}

{ #category : #public }
ReadOnlyUnixStore class >> isActiveClass [

	^ false
]

{ #category : #public }
ReadOnlyUnixStore class >> writableVariant [

	^ UnixStore
]

{ #category : #public }
ReadOnlyUnixStore >> basicOpen: aPath writable: aBoolean [

	aBoolean ifTrue: [ 
		(ReadOnlyFileException new
			messageText: 'Attempt to open file ' , aPath pathString, ' as writable on a read-only file system') signal.
		^ self ].
		
	^ super basicOpen: aPath writable: aBoolean. 
]

{ #category : #public }
ReadOnlyUnixStore >> createDirectory: path [

	(ReadOnlyFileException new
		messageText: 'Attempt crate directory ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyUnixStore >> delete: path [

	(ReadOnlyFileException new
		messageText: 'Attempt delete ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyUnixStore >> file: path posixPermissions: anInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyUnixStore >> file: path symlinkUid: uidInteger gid: gidInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyUnixStore >> file: path uid: uidInteger gid: gidInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyUnixStore >> handleClass [
	^ ReadOnlyFileHandle
]

{ #category : #public }
ReadOnlyUnixStore >> isWritable: aPath [

	^ false
]

{ #category : #public }
ReadOnlyUnixStore >> rename: sourcePath to: destinationPath [

	(ReadOnlyFileException new
		messageText: 'Attempt to rename file ' , sourcePath pathString, ' on a read-only file system') signal.
	
	^ self 
]
