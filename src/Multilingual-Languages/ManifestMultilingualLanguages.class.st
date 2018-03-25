Class {
	#name : #ManifestMultilingualLanguages,
	#superclass : #PackageManifest,
	#category : #'Multilingual-Languages'
}

{ #category : #'meta-data - dependency analyser' }
ManifestMultilingualLanguages class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Platforms')
]
