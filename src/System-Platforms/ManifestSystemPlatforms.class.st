Class {
	#name : #ManifestSystemPlatforms,
	#superclass : #PackageManifest,
	#category : #'System-Platforms'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemPlatforms class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
