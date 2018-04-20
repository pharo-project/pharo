Class {
	#name : #ManifestRegexCore,
	#superclass : #PackageManifest,
	#category : #'Regex-Core'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRegexCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
