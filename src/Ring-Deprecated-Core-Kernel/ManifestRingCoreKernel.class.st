Class {
	#name : #ManifestRingCoreKernel,
	#superclass : #PackageManifest,
	#category : #'Ring-Deprecated-Core-Kernel'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingCoreKernel class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Strings' #'Collections-Streams' #'System-Sources')
]
