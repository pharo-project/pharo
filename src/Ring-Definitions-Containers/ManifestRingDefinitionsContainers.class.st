"
Deprecated package for Ring Core containerss
"
Class {
	#name : #ManifestRingDefinitionsContainers,
	#superclass : #PackageManifest,
	#category : #'Ring-Definitions-Containers-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingDefinitionsContainers class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
