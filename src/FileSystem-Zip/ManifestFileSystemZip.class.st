"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestFileSystemZip,
	#superclass : #PackageManifest,
	#category : #'FileSystem-Zip'
}

{ #category : #'meta-data' }
ManifestFileSystemZip class >> description [ ^ 'This package contains the zip implementation of Pharo''s FileSystem manager. 

It make it possible to look over zip archives.

A documentation of this library is available in the Deep Into Pharo book, «Chapter 3: Files with FileSystem».'
]
