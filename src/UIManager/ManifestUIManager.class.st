"
Package for the UI Manager and related classes
"
Class {
	#name : 'ManifestUIManager',
	#superclass : 'PackageManifest',
	#category : 'UIManager-Manifest',
	#package : 'UIManager',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestUIManager class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]
