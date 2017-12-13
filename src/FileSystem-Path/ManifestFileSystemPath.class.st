"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestFileSystemPath,
	#superclass : #PackageManifest,
	#category : #'FileSystem-Path'
}

{ #category : #'meta-data' }
ManifestFileSystemPath class >> description [ ^ 'This package contains the path management of Pharo''s FileSystem manager. 

It is responsible for the modelisation of absolute and relative paths in the file system.

A documentation of this library is available in the Deep Into Pharo book, «Chapter 3: Files with FileSystem».'
]

{ #category : #'meta-data - dependency analyser' }
ManifestFileSystemPath class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
