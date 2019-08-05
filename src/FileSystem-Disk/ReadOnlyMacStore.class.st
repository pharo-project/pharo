"
I'm a specific store for macOS file systems that allows only read-only access. All read-only DiskStore subclasses share the same behavior. Usage of traits would be appropriate here, but it is not used because the kernel should not contain traits.
"
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
		ReadOnlyFileException signal: 'Attempt to open file ', aPath pathString, ' as writable on a read-only file system'.
		^ self ].
		
	^ super basicOpen: aPath writable: aBoolean. 
]

{ #category : #public }
ReadOnlyMacStore >> createDirectory: path [

	ReadOnlyFileException signal: 'Attempt crate directory ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> delete: path [

	ReadOnlyFileException signal: 'Attempt delete ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> file: path posixPermissions: anInteger [

	ReadOnlyFileException signal: 'Attempt to modify file ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> file: path symlinkUid: uidInteger gid: gidInteger [

	ReadOnlyFileException signal: 'Attempt to modify file ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> file: path uid: uidInteger gid: gidInteger [

	ReadOnlyFileException signal: 'Attempt to modify file ', path pathString, ' on a read-only file system'.
	
	^ self 
]

{ #category : #public }
ReadOnlyMacStore >> handleClass [
	^ ReadOnlyFileHandle
]

{ #category : #public }
ReadOnlyMacStore >> isWritable [

	^ false
]

{ #category : #public }
ReadOnlyMacStore >> isWritable: aPath [

	^ false
]

{ #category : #public }
ReadOnlyMacStore >> rename: sourcePath to: destinationPath [

	ReadOnlyFileException signal: 'Attempt to rename file ', sourcePath pathString, ' on a read-only file system'.
	
	^ self 
]
