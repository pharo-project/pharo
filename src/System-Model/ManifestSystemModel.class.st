Class {
	#name : #ManifestSystemModel,
	#superclass : #PackageManifest,
	#category : #'System-Model'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemModel class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
