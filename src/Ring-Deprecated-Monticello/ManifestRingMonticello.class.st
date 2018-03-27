Class {
	#name : #ManifestRingMonticello,
	#superclass : #PackageManifest,
	#category : #'Ring-Deprecated-Monticello'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingMonticello class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
