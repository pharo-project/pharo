"
Deprecated ring extensions for Monticello.
"
Class {
	#name : #ManifestRingMonticello,
	#superclass : #PackageManifest,
	#category : #'Ring-Deprecated-Monticello-Manifest'
}

{ #category : #deprecation }
ManifestRingMonticello class >> isDeprecated [
	^true
]

{ #category : #'meta-data - dependency analyser' }
ManifestRingMonticello class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
