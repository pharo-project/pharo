"
Different Encodings for Multilingual support
"
Class {
	#name : #ManifestMultilingualEncodings,
	#superclass : #PackageManifest,
	#category : #'Multilingual-Encodings-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestMultilingualEncodings class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
