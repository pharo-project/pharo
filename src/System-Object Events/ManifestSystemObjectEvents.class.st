"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemObjectEvents,
	#superclass : #PackageManifest,
	#category : #'System-Object Events'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemObjectEvents class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
