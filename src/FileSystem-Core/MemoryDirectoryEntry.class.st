"
Instances of this class represent a directory entry in a memory file system. So basically this class provides similar behavior as a DiscDirectoryEntry but for a memory file system.
"
Class {
	#name : #MemoryDirectoryEntry,
	#superclass : #FileSystemDirectoryEntry,
	#category : #'FileSystem-Core-Public'
}

{ #category : #accessing }
MemoryDirectoryEntry >> creationTime [

	^reference creationTime
]

{ #category : #accessing }
MemoryDirectoryEntry >> deviceId [
	"Simulate the device id with the file systemss hash"

	^reference store hash
]

{ #category : #accessing }
MemoryDirectoryEntry >> fileSize [
	"Simulate the device id with the file systems hash"

	^reference fileSize
]

{ #category : #accessing }
MemoryDirectoryEntry >> inode [
	"Simulate the inode with the file hash"

	^reference hash
]

{ #category : #testing }
MemoryDirectoryEntry >> isDirectory [

	^reference isDirectory
]

{ #category : #testing }
MemoryDirectoryEntry >> isFile [

	^reference isFile
]

{ #category : #testing }
MemoryDirectoryEntry >> isReadable [
	"The Memory File System doesn't have any access controls"

	^true
]

{ #category : #testing }
MemoryDirectoryEntry >> isWritable [
	"The Memory File System doesn't have any access controls"

	^true
]

{ #category : #accessing }
MemoryDirectoryEntry >> modificationTime [

	^reference modificationTime
]

{ #category : #accessing }
MemoryDirectoryEntry >> posixPermissions [
	"Answer the receivers posix permissions.
	The memory file system doesn't have any access controls"
	
	^8r777
]

{ #category : #accessing }
MemoryDirectoryEntry >> size [ 
	^reference size
]
