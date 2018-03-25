Class {
	#name : #ManifestRingMonticello,
	#superclass : #PackageManifest,
	#category : #'Ring-Monticello'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingMonticello class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
