"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemModel,
	#superclass : #PackageManifest,
	#category : 'System-Model'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemModel class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
