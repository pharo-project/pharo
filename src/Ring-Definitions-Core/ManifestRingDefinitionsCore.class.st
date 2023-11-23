"
Manifest for DEPRECATED package of Ring
"
Class {
	#name : 'ManifestRingDefinitionsCore',
	#superclass : 'PackageManifest',
	#category : 'Ring-Definitions-Core-Manifest',
	#package : 'Ring-Definitions-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestRingDefinitionsCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Streams' #'System-Sources')
]
