"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestRingCoreContainers,
	#superclass : #PackageManifest,
	#category : #'Ring-Core-Containers'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingCoreContainers class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
