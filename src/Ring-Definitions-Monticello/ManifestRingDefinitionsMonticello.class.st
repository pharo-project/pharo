"
Deprecated ring extensions for Monticello.
"
Class {
	#name : #ManifestRingDefinitionsMonticello,
	#superclass : #PackageManifest,
	#category : #'Ring-Definitions-Monticello-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingDefinitionsMonticello class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
