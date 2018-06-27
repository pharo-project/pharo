"
Support for regular expressions
"
Class {
	#name : #ManifestRegexCore,
	#superclass : #PackageManifest,
	#category : #'Regex-Core-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRegexCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
