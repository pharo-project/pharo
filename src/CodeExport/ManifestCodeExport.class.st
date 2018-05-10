"
Package manifest for package metadata
"
Class {
	#name : #ManifestCodeExport,
	#superclass : #PackageManifest,
	#category : #'CodeExport-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestCodeExport class >> manuallyResolvedDependencies [
	^ #(#'Slot-Core' #'System-Localization' #'FileSystem-Core' #'Collections-Abstract' #UIManager)
]
