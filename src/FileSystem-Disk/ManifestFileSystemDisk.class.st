"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestFileSystemDisk,
	#superclass : #PackageManifest,
	#category : #FileSystem-Disk
}

{ #category : #'meta-data - dependency analyser' }
ManifestFileSystemDisk class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
