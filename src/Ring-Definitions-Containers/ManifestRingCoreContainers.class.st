"
Deprecated package for Ring Core containerss
"
Class {
	#name : #ManifestRingCoreContainers,
	#superclass : #PackageManifest,
	#category : #'Ring-Definitions-Containers-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingCoreContainers class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
