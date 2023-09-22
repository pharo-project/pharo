Class {
	#name : 'ManifestSystemModel',
	#superclass : 'PackageManifest',
	#category : 'System-Model-Manifest',
	#package : 'System-Model',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemModel class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
