Class {
	#name : #ManifestMultilingualTextConversion,
	#superclass : #PackageManifest,
	#category : #'Multilingual-TextConversion'
}

{ #category : #'meta-data - dependency analyser' }
ManifestMultilingualTextConversion class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Streams')
]
