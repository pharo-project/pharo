"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemPlatforms,
	#superclass : #PackageManifest,
	#category : #'System-Platforms'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemPlatforms class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract')
]
