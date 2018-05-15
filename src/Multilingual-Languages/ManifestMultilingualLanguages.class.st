"
Support for different language environments (Latin1, Latin2, Latin9)
"
Class {
	#name : #ManifestMultilingualLanguages,
	#superclass : #PackageManifest,
	#category : #'Multilingual-Languages-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestMultilingualLanguages class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Platforms')
]
