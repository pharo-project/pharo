"
Implementation for a new value holder
"
Class {
	#name : #ManifestNewValueHolder,
	#superclass : #PackageManifest,
	#category : #'NewValueHolder-Core-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestNewValueHolder class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]
