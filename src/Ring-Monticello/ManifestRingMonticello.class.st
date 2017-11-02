"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestRingMonticello,
	#superclass : #PackageManifest,
	#category : 'Ring-Monticello'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingMonticello class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
