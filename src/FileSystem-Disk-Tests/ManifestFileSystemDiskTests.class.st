"
This package contains the disk tests of Pharo's FileSystem manager.

A documentation of this library is available in the Deep Into Pharo book, «Chapter 3: Files with FileSystem».
"
Class {
	#name : 'ManifestFileSystemDiskTests',
	#superclass : 'PackageManifest',
	#category : 'FileSystem-Disk-Tests-Manifest',
	#package : 'FileSystem-Disk-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestFileSystemDiskTests class >> ruleAssertEqualSignIntoAssertEqualsV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#DiskFileSystemTest #testEqual #false)) #'2019-03-29T13:41:22.736554+01:00') )
]
