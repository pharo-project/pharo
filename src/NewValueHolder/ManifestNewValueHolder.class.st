Class {
	#name : #ManifestNewValueHolder,
	#superclass : #PackageManifest,
	#category : #NewValueHolder
}

{ #category : #'meta-data - dependency analyser' }
ManifestNewValueHolder class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]
