"
This package contains the core of Pharo's FileSystem manager. 

It offers an expressive and elegant object-oriented design. 

A documentation of this library is available in the Deep Into Pharo book, «Chapter 3: Files with FileSystem».
"
Class {
	#name : #ManifestFileSystemCore,
	#superclass : #PackageManifest,
	#category : #'FileSystem-Core-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestFileSystemCore class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'System-Platforms' #'Zinc-Character-Encoding-Core' #'AST-Core' #'System-Settings-Core' #'FileSystem-Disk')
]
