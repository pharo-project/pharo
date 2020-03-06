"
Manifest for DEPRECATED package of Ring
"
Class {
	#name : #ManifestRingDefinitionsCore,
	#superclass : #PackageManifest,
	#category : #'Ring-Definitions-Core-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingDefinitionsCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Strings' #'Collections-Streams' #'System-Sources')
]
