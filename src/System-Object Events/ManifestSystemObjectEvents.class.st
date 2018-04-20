Class {
	#name : #ManifestSystemObjectEvents,
	#superclass : #PackageManifest,
	#category : #'System-Object Events'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemObjectEvents class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
