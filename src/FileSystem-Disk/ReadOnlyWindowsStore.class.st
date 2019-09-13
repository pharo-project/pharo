"
I'm a specific store for Windows file systems that allows only read-only access. All read-only DiskStore subclasses share the same behavior. Usage of traits would be appropriate here, but it is not used because the kernel should not contain traits.
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
		ReadOnlyFileException signal: 'Attempt to open file ' , aPath pathString, ' as writable on a read-only file system'.
		^ self ].
		
	^ super basicOpen: aPath writable: aBoolean. 
]

{ #category : #public }
ReadOnlyWindowsStore >> createDirectory: path [

	ReadOnlyFileException signal: 'Attempt crate directory ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> delete: path [

	ReadOnlyFileException signal: 'Attempt delete ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> file: path posixPermissions: anInteger [

	ReadOnlyFileException signal: 'Attempt to modify file ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> file: path symlinkUid: uidInteger gid: gidInteger [

	ReadOnlyFileException signal: 'Attempt to modify file ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> file: path uid: uidInteger gid: gidInteger [

	ReadOnlyFileException signal: 'Attempt to modify file ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyWindowsStore >> handleClass [
	^ ReadOnlyFileHandle
]

{ #category : #public }
ReadOnlyWindowsStore >> isWritable [

	^ false
]

{ #category : #public }
ReadOnlyWindowsStore >> isWritable: aPath [

	^ false
]

{ #category : #public }
ReadOnlyWindowsStore >> rename: sourcePath to: destinationPath [

	ReadOnlyFileException signal: 'Attempt to rename file ', sourcePath pathString, ' on a read-only file system'.
	
	^ self 
]
