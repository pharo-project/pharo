"
Support for different OS platforms
"
Class {
	#name : #ManifestSystemPlatforms,
	#superclass : #PackageManifest,
	#category : #'System-Platforms-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemPlatforms class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
