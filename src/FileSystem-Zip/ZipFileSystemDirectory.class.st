"
I represent a zip file system entry for a directory. Together with ZipFileSystemFile I take care of lazily accessing archive members.
"
Class {
	#name : #ZipFileSystemDirectory,
	#superclass : #MemoryFileSystemDirectory,
	#category : #'FileSystem-Zip'
}

{ #category : #creation }
ZipFileSystemDirectory >> ensureCreateFile: aFileName [
	^ self 
		fileEntryAt: aFileName 
		put: (ZipFileSystemFile named: aFileName)
]
