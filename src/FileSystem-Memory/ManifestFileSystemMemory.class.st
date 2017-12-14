"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestFileSystemMemory,
	#superclass : #PackageManifest,
	#category : #'FileSystem-Memory'
}

{ #category : #'meta-data' }
ManifestFileSystemMemory class >> description [ ^ 'This package contains a memory file system part of Pharo''s FileSystem manager. 

It allow to emulate a file system directly into the memory of the computer. This memory file system can be used during tests for two reasons:
- To avoid to write temporary files (it will also help when Pharo is stored in a read only environment)
- To speed up the tests executions

A documentation of this library is available in the Deep Into Pharo book, «Chapter 3: Files with FileSystem».'
]
