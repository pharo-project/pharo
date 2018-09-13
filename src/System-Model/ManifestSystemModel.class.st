Class {
	#name : #ManifestSystemModel,
	#superclass : #PackageManifest,
	#category : #'System-Model-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemModel class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
