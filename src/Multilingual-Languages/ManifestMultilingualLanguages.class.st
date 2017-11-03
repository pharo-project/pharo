"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestMultilingualLanguages,
	#superclass : #PackageManifest,
	#category : #Multilingual-Languages
}

{ #category : #'meta-data - dependency analyser' }
ManifestMultilingualLanguages class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Platforms')
]
