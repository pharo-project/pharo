"
I represent a memory file system entry for a directory
"
Class {
	#name : #MemoryFileSystemDirectory,
	#superclass : #MemoryFileSystemEntry,
	#instVars : [
		'entries'
	],
	#category : #'FileSystem-Memory-Base'
}

{ #category : #creation }
MemoryFileSystemDirectory >> ensureCreateDirectory: aDirectoryName [
	^ self 
		fileEntryAt: aDirectoryName
		put: (self class named: aDirectoryName)
]

{ #category : #creation }
MemoryFileSystemDirectory >> ensureCreateFile: aFileName [
	^ self 
		fileEntryAt: aFileName 
		put: (MemoryFileSystemFile named: aFileName)
]

{ #category : #accessing }
MemoryFileSystemDirectory >> entries [
	^ entries
]

{ #category : #enumeration }
MemoryFileSystemDirectory >> fileEntriesDo: aBlock [
	
	entries keys sort do: [ :fileName|
		aBlock value: (entries at: fileName)].
]

{ #category : #accessing }
MemoryFileSystemDirectory >> fileEntriesIncludes: aFileName [
	^ entries includesKey: aFileName
]

{ #category : #accessing }
MemoryFileSystemDirectory >> fileEntryAt: aFileName [
	^ entries at: aFileName
]

{ #category : #accessing }
MemoryFileSystemDirectory >> fileEntryAt: aFileName ifAbsent: aBlock [
	^ entries at: aFileName ifAbsent: aBlock
]

{ #category : #accessing }
MemoryFileSystemDirectory >> fileEntryAt: aFileName ifPresent: aBlock [
	^ entries at: aFileName ifPresent: aBlock
]

{ #category : #accessing }
MemoryFileSystemDirectory >> fileEntryAt: aFileName put: anEntry [
	^ entries 
		at: aFileName 
		ifAbsentPut: [
			self modified.
			anEntry ]
]

{ #category : #accessing }
MemoryFileSystemDirectory >> fileEntryRemove: aFileName [
	^ self fileEntryRemove: aFileName ifAbsent: [ FileDoesNotExist signalWith: aFileName ]
]

{ #category : #accessing }
MemoryFileSystemDirectory >> fileEntryRemove: aFileName ifAbsent: absentBlock [
	| deletedEntry |
	deletedEntry := entries removeKey: aFileName ifAbsent: [ ^ absentBlock value ].
	modificationTime := DateAndTime now.
	^ deletedEntry
]

{ #category : #accessing }
MemoryFileSystemDirectory >> fileSize [
	^ 0
]

{ #category : #initialization }
MemoryFileSystemDirectory >> initialize [
	super initialize.
	entries := Dictionary new.
]

{ #category : #testing }
MemoryFileSystemDirectory >> isDirectory [
	^ true
]
