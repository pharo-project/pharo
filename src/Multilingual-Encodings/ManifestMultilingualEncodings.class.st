"
Different Encodings for Multilingual support
"
Class {
	#name : 'ManifestMultilingualEncodings',
	#superclass : 'PackageManifest',
	#category : 'Multilingual-Encodings-Manifest',
	#package : 'Multilingual-Encodings',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMultilingualEncodings class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
