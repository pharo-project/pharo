"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestFileSystemPath,
	#superclass : #PackageManifest,
	#category : #'FileSystem-Path'
}

{ #category : #'meta-data - dependency analyser' }
ManifestFileSystemPath class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
