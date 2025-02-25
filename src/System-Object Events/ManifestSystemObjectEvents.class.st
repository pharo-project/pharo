"
System support for object events (change/update mechanism)
"
Class {
	#name : 'ManifestSystemObjectEvents',
	#superclass : 'PackageManifest',
	#category : 'System-Object Events-Manifest',
	#package : 'System-Object Events',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemObjectEvents class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
