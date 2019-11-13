"
A package providing Universally Unique Identifiers (UUID) following RFC4122.
"
Class {
	#name : #ManifestNetworkUUID,
	#superclass : #PackageManifest,
	#category : #'Network-UUID-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestNetworkUUID class >> ignoredDependencies [
	^ #(#'Network-Kernel')
]

{ #category : #'meta-data - dependency analyser' }
ManifestNetworkUUID class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
