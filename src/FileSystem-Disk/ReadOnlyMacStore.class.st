Class {
	#name : #ReadOnlyMacStore,
	#superclass : #MacStore,
	#category : #'FileSystem-Disk-Store'
}

{ #category : #public }
ReadOnlyMacStore class >> isActiveClass [

	^ false
]

{ #category : #public }
ReadOnlyMacStore class >> writableVariant [

	^ MacStore
]

{ #category : #public }
ReadOnlyMacStore >> basicOpen: aPath writable: aBoolean [

	aBoolean ifTrue: [ 
		(ReadOnlyFileException new
			messageText: 'Attempt to open file ' , aPath pathString, ' as writable on a read-only file system') signal.
		^ self ].
		
	^ super basicOpen: aPath writable: aBoolean. 
]

{ #category : #public }
ReadOnlyMacStore >> createDirectory: path [

	(ReadOnlyFileException new
		messageText: 'Attempt crate directory ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> delete: path [

	(ReadOnlyFileException new
		messageText: 'Attempt delete ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> file: path posixPermissions: anInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> file: path symlinkUid: uidInteger gid: gidInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> file: path uid: uidInteger gid: gidInteger [

	(ReadOnlyFileException new
		messageText: 'Attempt to modify file ' , path pathString, ' on a read-only file system') signal.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> handleClass [
	^ ReadOnlyFileHandle
]

{ #category : #public }
ReadOnlyMacStore >> isWritable: aPath [

	^ false
]

{ #category : #public }
ReadOnlyMacStore >> rename: sourcePath to: destinationPath [

	(ReadOnlyFileException new
		messageText: 'Attempt to rename file ' , sourcePath pathString, ' on a read-only file system') signal.
	
	^ self 
]
