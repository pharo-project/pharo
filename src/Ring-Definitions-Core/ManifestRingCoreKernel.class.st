"
Manifest for DEPRECATED package of Ring
"
Class {
	#name : #ManifestRingCoreKernel,
	#superclass : #PackageManifest,
	#category : #'Ring-Definitions-Core-Manifest'
}

{ #category : #deprecation }
ManifestRingCoreKernel class >> isDeprecated [
	^true
]

{ #category : #'meta-data - dependency analyser' }
ManifestRingCoreKernel class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract' #'Collections-Strings' #'Collections-Streams' #'System-Sources')
]
