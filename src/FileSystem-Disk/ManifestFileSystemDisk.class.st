"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestFileSystemDisk,
	#superclass : #PackageManifest,
	#category : #'FileSystem-Disk'
}

{ #category : #'meta-data' }
ManifestFileSystemDisk class >> description [ ^ 'This package contains the disk management of Pharo''s FileSystem manager. 

Its responsibility is to provid a common API for the different OS''s disks. 

A documentation of this library is available in the Deep Into Pharo book, «Chapter 3: Files with FileSystem».'
]

{ #category : #'meta-data - dependency analyser' }
ManifestFileSystemDisk class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
