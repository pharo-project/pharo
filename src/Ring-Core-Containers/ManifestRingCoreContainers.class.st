Class {
	#name : #ManifestRingCoreContainers,
	#superclass : #PackageManifest,
	#category : #'Ring-Core-Containers'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingCoreContainers class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
