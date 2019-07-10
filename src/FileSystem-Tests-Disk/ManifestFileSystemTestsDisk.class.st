"
This package contains the disk tests of Pharo's FileSystem manager.

A documentation of this library is available in the Deep Into Pharo book, «Chapter 3: Files with FileSystem».
"
Class {
	#name : #ManifestFileSystemTestsDisk,
	#superclass : #PackageManifest,
	#category : #'FileSystem-Tests-Disk-Manifest'
}

{ #category : #'code-critics' }
ManifestFileSystemTestsDisk class >> ruleAssertEqualSignIntoAssertEqualsV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#DiskFileSystemTest #testEqual #false)) #'2019-03-29T13:41:22.736554+01:00') )
]
