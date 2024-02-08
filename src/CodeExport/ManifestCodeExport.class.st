"
Package manifest for package metadata
"
Class {
	#name : 'ManifestCodeExport',
	#superclass : 'PackageManifest',
	#category : 'CodeExport-Manifest',
	#package : 'CodeExport',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestCodeExport class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
