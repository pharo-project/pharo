Class {
	#name : #ManifestSystemChanges,
	#superclass : #PackageManifest,
	#category : #'System-Changes'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemChanges class >> manuallyResolvedDependencies [
	^ #(#CodeExport #'OpalCompiler-Core' #'FileSystem-Disk' #'Collections-Abstract')
]
