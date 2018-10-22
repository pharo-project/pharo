"
Packacke with support for system changes recorded in changesets
"
Class {
	#name : #ManifestSystemChanges,
	#superclass : #PackageManifest,
	#category : #'System-Changes-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemChanges class >> manuallyResolvedDependencies [
	^ #(#CodeExport #'OpalCompiler-Core' #'FileSystem-Disk' #'Collections-Abstract')
]
