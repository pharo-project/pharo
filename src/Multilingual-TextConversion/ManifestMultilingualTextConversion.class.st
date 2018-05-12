"
Package for multilingual text conversion classes
"
Class {
	#name : #ManifestMultilingualTextConversion,
	#superclass : #PackageManifest,
	#category : #'Multilingual-TextConversion-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestMultilingualTextConversion class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Streams')
]
