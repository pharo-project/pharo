"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestMultilingualTextConversion,
	#superclass : #PackageManifest,
	#category : #Multilingual-TextConversion
}

{ #category : #'meta-data - dependency analyser' }
ManifestMultilingualTextConversion class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Streams')
]
