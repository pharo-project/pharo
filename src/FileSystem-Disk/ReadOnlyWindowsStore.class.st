"
a Windows store that allows only read-only access
"
Class {
	#name : #ReadOnlyWindowsStore,
	#superclass : #WindowsStore,
	#category : #'FileSystem-Disk-Store'
}

{ #category : #public }
ReadOnlyWindowsStore class >> isActiveClass [

	^ false
]

{ #category : #public }
ReadOnlyWindowsStore class >> writableVariant [

	^ WindowsStore
]

{ #category : #public }
ReadOnlyWindowsStore >> basicOpen: aPath writable: aBoolean [

	aBoolean ifTrue: [ 
		(ReadOnlyFileException new
			messageText: 'Attempt to open file ' , aPath pathString, ' as writable on a read-only file system') signal.
		^ self ].
		
	^ super basicOpen: aPath writable: aBoolean. 
]

{ #category : #public }
ReadOnlyWindowsStore >> createDirectory: path [

	(ReadOnlyFileException new
		messageText: 'Attempt crate directory ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> delete: path [

	(ReadOnlyFileException new
		messageText: 'Attempt delete ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> file: path posixPermissions: anInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> file: path symlinkUid: uidInteger gid: gidInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> file: path uid: uidInteger gid: gidInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> handleClass [
	^ ReadOnlyFileHandle
]

{ #category : #public }
ReadOnlyWindowsStore >> isWritable: aPath [

	^ false
]

{ #category : #public }
ReadOnlyWindowsStore >> rename: sourcePath to: destinationPath [

	(ReadOnlyFileException new
		messageText: 'Attempt to rename file ' , sourcePath pathString, ' on a read-only file system') signal.
	
	^ self 
]
