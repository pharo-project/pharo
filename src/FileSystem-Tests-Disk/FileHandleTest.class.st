"
SUnit tests for file handles, the tests may be found in superclass
"
Class {
	#name : #FileHandleTest,
	#superclass : #FileSystemHandleTest,
	#category : #'FileSystem-Tests-Disk-Base'
}

{ #category : #running }
FileHandleTest >> createFileSystem [
	^ FileSystem store: DiskStore activeClass createDefault
]
