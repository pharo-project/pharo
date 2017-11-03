"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemChanges,
	#superclass : #PackageManifest,
	#category : #System-Changes
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemChanges class >> manuallyResolvedDependencies [
	^ #(#CodeExport #'OpalCompiler-Core' #'FileSystem-Disk' #'Collections-Abstract')
]
